
import requests
from bs4 import BeautifulSoup as bs
import time
import random
import json
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import re
from datetime import date
import os
import yaml

with open('secrets.yaml', 'r') as secrets_file:
    secrets = yaml.safe_load(secrets_file)
headers = secrets['headers']


def get_ids(headers, cp):
    '''
    Function that returns all the flats ids of certain zip code with fixed headers request.
    The function return a list with the ids.
    
    headers: needed to obtain response 200 (success).
    cp: zip code.
    
    Returns list of ids.
    '''
    ids = []

    with requests.Session() as session:

        # Get the first page of results
        url = f'https://www.idealista.com/buscar/alquiler-viviendas/{cp}/'
        r = session.get(url, headers=headers)
        r.raise_for_status()
        soup = bs(r.text, 'lxml')
        articles = soup.find_all('article', {'class': 'item'})
        ids += [article.get('data-adid') for article in articles]

        x=2
        
    while True:
        url = f'https://www.idealista.com/buscar/alquiler-viviendas/{cp}/pagina-{x}.htm'
        r = session.get(url, headers=headers)
        if r.status_code != 200:
            break
        soup = bs(r.text, 'lxml')

        if soup.find('main', {'class':'listing-items'}).find('div',{'class':'pagination'}) == None:
            break


        pag_actual = int(soup.find('main', {'class':'listing-items'}).find('div',{'class':'pagination'}).find('li',{'class':'selected'}).text)

#         print(x,pag_actual)
        if x == pag_actual:
                    articles = soup.find('main', {'class':'listing-items'}).find_all('article')
        else:
            break
        articles = soup.find_all('article', {'class': 'item'})
        if not articles:
            break
        ids += [article.get('data-adid') for article in articles]
        x += 1
        time.sleep(random.randint(3, 4) * random.random())
        
    return ids


def obtain_data_from_ids(ids, headers, cp):
    '''
    Function that return all the data from each add id from previous function "get_ids".
    It returns the data in dictionary format, ready to be converted to dataframe.

    ids = list of ids.
    headers = to avoid errors, response 200 (success)
    cp = zip code of the ids

    Returns a dic with id as a key.
    '''

    dic = {}
    url = "https://www.idealista.com/inmueble/{}/"
    
    regex_m2 = re.compile(r'\d\d m²|\d\d\d m²')
    regex_hab = re.compile(r'\d hab|\d\d hab|sin habitación')
    regex_baño = re.compile(r'\d baño')
    regex_planta = re.compile(r'planta \d\d|planta \d|Entreplanta|bajo')
    
    x=1
    
    for i in ids:
#        print("aqui entra en el bucle")
        with requests.Session() as session:
            try:
#               print("aqui esta en el try")
                r = session.get(url.format(i), headers=headers)
                r.raise_for_status()

                soup = bs(r.text, 'lxml')  # formato mas legible

                # ubicacion
                ubi = soup.select("#headerMap li")
                ubicacion_full = '|'.join([u.get_text(strip=True) for u in ubi])  # string

                titulo = soup.select_one('h1').get_text(strip=True)
                localizacion = soup.select_one('.main-info__title-block span').get_text(strip=True)

                # ubi table
                ubicacion_items = [re.sub(r'\n+', '', u.get_text(strip=True)) for u in ubi]

                distrito2 = next((i for i in ubicacion_items if 'Distrito' in i), None)
                
                barrio2 = next((i for i in ubicacion_items if 'Barrio' in i), None)

                calle = ubicacion_items[0]
                
                barrio = ubicacion_items[1]

                distrito = ubicacion_items[2]
         
                area = ubicacion_items[4]
                precio = soup.select_one('.info-data span').get_text(strip=True).strip(' €/mes')

                if "." in precio:
                    precio = precio.replace('.', '')  # falta formato int

                precio_down = soup.select_one(".pricedown_price")
                  
                if precio_down != None :
                    precio_down = precio_down.text.strip("\n").strip(" €").replace('.','')
                else:
                    precio_down = precio
                
#                 detalles basicos del piso habitaciones, baños, etc
                detalles = soup.find('section',{"id":"details"}).find("div",{"class":"details-property-feature-one"})
                # info sobre aire acondicionado, psicina, zonas verdes y consumo energetico
                detalles2 = soup.find('section',{"id":"details"}).find("div",{"class":"details-property-feature-two"})
                caract = [det.text.strip() for det in detalles.find_all("li")]

                s = ' '.join(caract).lower()

                metros = regex_m2.search(s).group().replace(' m²','')

                hab =  regex_hab.search(s).group()

                wc =  regex_baño.search(s).group()
        
                terraza = "".join(re.findall(r'terraza',s))
                balcon = "".join(re.findall(r'balcón|balcon',s))
                estado = "".join(re.findall(r'segunda mano/buen estado',s))
                year = "".join(re.findall(r'\d\d\d\d',s))
                armarios = "".join(re.findall(r'armarios',s))
                cocina = "".join(re.findall(r'cocina equipada|cocina sin equipar',s))
                amu = "".join(re.findall(r'amueblada|amueblado|sin amueblar',s))

                if regex_planta.search(s) == None:
                    planta = 0
                else:
                    planta = regex_planta.search(s).group()
    
                calefac = "".join(re.findall(r'no dispone de calefacción|calefacción',s))
                ascn = "".join(re.findall(r'con ascensor|sin ascensor',s))
                aire = detalles2.find('ul').text.strip("\n").lower()
                
                if aire == "aire acondicionado":
                    aire = 1
                else:
                    aire = 0
                exterior = "".join(re.findall(r'exterior|interior',s))
                datalles2 = "".join([det.text.replace("\n","|").lower() for det  in detalles2.find_all('ul')]) # añado zonas verdes, psicina
                cp = cp
                actualizacion = soup.find('p',{"class":"date-update-text"}).text
  
                actualizacion2 = soup.find("p", {"class":"stats-text"}).text
                extract_day = date.today()

                dic[i] = [titulo,localizacion,ubicacion_full,distrito2,calle,barrio,barrio2,distrito,area,precio,precio_down,metros,hab,wc,terraza,balcon,estado,year,armarios,cocina,amu,planta,calefac,ascn,aire,exterior,datalles2,cp,actualizacion,actualizacion2,extract_day]
                   
                len_ids = len(ids)
                print(r, "anuncio número:{}/{} cp: {}".format(x,len_ids,cp),sep="\n")
                
                x += 1
                
                time.sleep(random.randint(4,6)*random.random())
        
            except Exception as e:
                print(f"error {e}")
        
#                 dic[i] = ["error"] * 50
                          
    return dic


def dic_to_df(dic):
    '''
    Transform dic from Idealista https://www.idealista.com/inmueble
    to a data frame.
    
    dic -> Dataframe
    '''
    colnames = ['name','zone','ubicacion_full','distrito2','calle','barrio','barrio2','distrito','area','price','price_before','square_mt','rooms','wc','terraza','balcon','estado',     'año','armarios','cocina','amueblado','planta','calef','asc','aire','exterior','datalles2','cp','actualizacion','actualizacion2','extract_day']
    df = pd.DataFrame.from_dict(dic, orient='index',columns=colnames)
    
    return df


def df_parse(df):
    '''
    Transform dataframe from Idealista https://www.idealista.com/inmueble
    to a clean data frame and convert the data types to int when possible.
    
    Dataframe -> Dataframe
    '''
    df['price'] = df.price.astype(int)
    df['price_before'] = df.price_before.astype(int)
    df['square_mt'] = df.square_mt.astype(int)
    try:
        df ['rooms']=df.rooms.apply(lambda x: x.replace('sin habitación','0'))
        df ['rooms']=df.rooms.apply(lambda x: x.replace(' hab','')).astype(int)
    except:
        pass
    try:
        df ['wc']=df.wc.apply(lambda x: x.replace(' baño','')).astype(int)
    #     df ['wc']=df.rooms.apply(lambda x: x.replace(' hab','')).astype(int)
    except:
        pass
    df['terraza'] = df.terraza.apply(lambda x:  x.replace('terraza','1') if x == 'terraza' else '0').astype(int)
    df['balcon'] = df.balcon.apply(lambda x:  x.replace('balcón','1') if x == 'balcón' else '0').astype(int)
    df['armarios'] = df.armarios.apply(lambda x:  x.replace('armarios','1') if x == 'armarios' else '0').astype(int)
    df['cocina'] = df.cocina.apply(lambda x:  x.replace('cocina equipada','1') if x == 'cocina equipada' else '0').astype(int)
    df['amueblado'] = df.amueblado.apply(lambda x:  x.replace('amueblado','1') if x == 'amueblado' else '0').astype(int)
    df['calef'] = df.calef.apply(lambda x:  x.replace('calefacción','1') if x == 'calefacción' else '0').astype(int)
    df['asc'] = df.asc.apply(lambda x:  x.replace('con ascensor','1') if x == 'con ascensor' else '0').astype(int)
    df['aire'] = df.aire.astype(int)
    df['exterior'] = df.exterior.apply(lambda x:  x.replace('exterior','1') if x == 'exterior' else '0').astype(int)
    try:
        df['planta'] = df.planta.apply(lambda x: x.replace('planta ',''))
        df['planta'] = df.planta.apply(lambda x: np.nan if x == "" else x)
        df['planta'] = df.planta.apply(lambda x: '0' if x == "bajo" else x)
        df['planta'] = df.planta.apply(lambda x: '1' if x == "Entreplanta" else x)
        df['planta'] = df.planta.astype(int)
    except:
        pass
    df['cp'] = df.cp.astype(str)
    
    return df

def create_folder(parent_path):
    
    today = date.today()
    directory = "extraction_{}".format(today)
    
    list_files = os.listdir(parent_path)
#     path = os.path.join(parent_path, directory)
    
    if directory not in list_files:
        path = os.path.join(parent_path, directory)
        os.mkdir(path)
#         os.close(path)
        print("Directory '% s' created" % directory)
    else:
#         os.close(path)
        print("Directory '% s' already created" % directory)
    
    return directory
        

def chunk(lst, n):
    """Yield successive n-sized chunks from lst."""
    for i in range(0, len(lst), n):
        yield lst[i:i + n]
        

def call_scrapper(cp_list,parent_path): # poner headers aqui
    
    print("parent directory: ",os.getcwd())
    
    new_dir_name = create_folder(parent_path)
    
    os.chdir("{}\\{}".format(parent_path,new_dir_name))

    print("csv folder: ",os.getcwd())
    
    for chunked_list in chunk(cp_list, 4):
        tmp_cp_list = chunked_list
        print(tmp_cp_list)
    
        for ind, zip_code in enumerate(tmp_cp_list):
            print("buscando:",zip_code)
            id_from_cp = get_ids(headers, zip_code)
            time.sleep(random.randint(2,4)*random.random())
            dic_data = obtain_data_from_ids(id_from_cp,headers,cp = zip_code)
            time.sleep(random.randint(2,4)*random.random())
            df_data = df_parse(dic_to_df(dic_data))

            df_data.to_csv("data_{}.csv".format(zip_code),encoding = 'utf-8-sig',index_label= "id")
    #         df_final = df_final.append(df_data)

        #     cp.pop(0) da problemas
        #     Para saber que cp han sido buscados se agregan a cp_buscados

    #         cp_buscados.append(zip_code)

            print("guardado datos de:",zip_code)
            time.sleep(random.randint(2,4)*random.random()) 

        if "08042" not in tmp_cp_list: # no esperar despues de la ultima extraccion
            time.sleep(random.randint(800,900))

# Moved to utils.py
# def concat_csv(parent_path, folder=None):
    
#     colnames = ['id','name','zone','ubicacion_full','calle','barrio','barrio2','distrito','area','price','price_before','square_mt','rooms','wc','terraza','balcon','estado',     'año','armarios','cocina','amueblado','planta','calef','asc','aire','exterior','datalles2','cp','actualizacion','actualizacion2','extract_day']
#     df_final = pd.DataFrame(columns = colnames)
    
#     if folder is None:
#         day_save = date.today()
#         directory = "extraction_{}".format(today)
#         path = os.path.join(parent_path, directory)
#         try:
#             os.chdir(path)
#             print("path exists")
#             csv = os.listdir()
#         except ValueError:
#             print("path does not exists")

        
    
#     if folder is not None:
#         day_save = re.search('\d{4}-\d{2}-\d{2}', folder)[0]
#         path = os.path.join(parent_path, folder)
#         try:
#             os.chdir(path)
#             print("path exists")
#             csv = os.listdir()
#         except ValueError:
#             print("path does not exists")
            
        
        
#     for i in csv:
#         df = pd.read_csv(i)
#         df_final = pd.concat([df_final,df],axis=0,ignore_index=True)
        
#     df_final.to_csv("datos_scrapping_{}.csv".format(day_save),encoding = 'utf-8-sig',index=False)
#     print("file '% s' created" % "datos_scrapping_{}.csv".format(day_save))
        
    


