
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


# headers -> Ctrl + shift + i (pagina de desarrollador) -> network Ctrl + f5 -> primera pestaña -> request headers
# desde "accept" ponerlo en dict 
headers = {
"accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
"accept-encoding": "gzip, deflate, br",
"accept-language": "en-GB,en;q=0.9,en-US;q=0.8,ca;q=0.7,es;q=0.6,eo;q=0.5",
"cache-control": "no-cache",
"cookie": '''atuserid=%7B%22name%22%3A%22atuserid%22%2C%22val%22%3A%2268f673fe-430e-424d-9df0-0cd8699a34a7%22%2C%22options%22%3A%7B%22end%22%3A%222023-03-03T18%3A46%3A49.424Z%22%2C%22path%22%3A%22%2F%22%7D%7D; atidvisitor=%7B%22name%22%3A%22atidvisitor%22%2C%22val%22%3A%7B%22vrn%22%3A%22-582065-%22%7D%2C%22options%22%3A%7B%22path%22%3A%22%2F%22%2C%22session%22%3A15724800%2C%22end%22%3A15724800%7D%7D; didomi_token=eyJ1c2VyX2lkIjoiMTdlYWM0ZmEtNzcxNC02MGMyLWJmODMtYWRjMGViMzFiYzE3IiwiY3JlYXRlZCI6IjIwMjItMDEtMzBUMTg6NDY6NTEuMTA0WiIsInVwZGF0ZWQiOiIyMDIyLTAxLTMwVDE4OjQ2OjUxLjEwNFoiLCJ2ZW5kb3JzIjp7ImVuYWJsZWQiOlsiZ29vZ2xlIiwiYzptaXhwYW5lbCIsImM6YWJ0YXN0eS1MTGtFQ0NqOCIsImM6aG90amFyIiwiYzp5YW5kZXhtZXRyaWNzIiwiYzpiZWFtZXItSDd0cjdIaXgiLCJjOmFwcHNmbHllci1HVVZQTHBZWSIsImM6dGVhbGl1bWNvLURWRENkOFpQIiwiYzppZGVhbGlzdGEtTHp0QmVxRTMiLCJjOmlkZWFsaXN0YS1mZVJFamUyYyJdfSwicHVycG9zZXMiOnsiZW5hYmxlZCI6WyJhbmFseXRpY3MtSHBCSnJySzciLCJnZW9sb2NhdGlvbl9kYXRhIl19LCJ2ZXJzaW9uIjoyLCJhYyI6IkFGbUFDQUZrLkFBQUEifQ==; euconsent-v2=CPTmlIAPTmlIAAHABBENB_CoAP_AAAAAAAAAF5wBAAIAAtAC2AvMAAABAaADAAEEQyUAGAAIIhlIAMAAQRDIQAYAAgiGOgAwABBEMJABgACCIYyADAAEEQxUAGAAIIhg.f_gAAAAAAAAA; _gcl_au=1.1.8146483.1643568415; _fbp=fb.1.1643568415299.2070506742; afUserId=58c2a54c-4375-4eb4-baca-6a6e48c65725-p; AF_SYNC=1643568416416; askToSaveAlertPopUp=true; _hjSessionUser_250321=eyJpZCI6ImU0N2FkYjkyLTgyMDMtNWU0Mi1iMTRiLWIzMzQzMDE0NTM0NiIsImNyZWF0ZWQiOjE2NDM1Njg1MjgzMzYsImV4aXN0aW5nIjp0cnVlfQ==; userUUID=6eadbb28-4981-4c13-abda-fd7fadaec832; SESSION=f1f80941eb4fade2~bda569d4-9873-4a74-80a2-5fd0ae1a6c25; contactbda569d4-9873-4a74-80a2-5fd0ae1a6c25="{'email':null,'phone':null,'phonePrefix':null,'friendEmails':null,'name':null,'message':null,'message2Friends':null,'maxNumberContactsAllow':10,'defaultMessage':true}"; cookieSearch-1="/alquiler-viviendas/barcelona/les-corts/les-corts/:1643650811421"; sendbda569d4-9873-4a74-80a2-5fd0ae1a6c25="{'friendsEmail':null,'email':null,'message':null}"; _hjIncludedInSessionSample=1; _hjSession_250321=eyJpZCI6IjYyNTE2ZTJjLWNkNTYtNDRkZS05NmJkLWQ4ZjYxYjQwMGE1NCIsImNyZWF0ZWQiOjE2NDM2NTEzNjU5MTEsImluU2FtcGxlIjp0cnVlfQ==; _hjAbsoluteSessionInProgress=0; _hjCachedUserAttributes=eyJhdHRyaWJ1dGVzIjp7ImlkX3BhZ2VMYW5ndWFnZSI6ImVzIiwiaWRfdXNlclJvbGUiOiIifSwidXNlcklkIjpudWxsfQ==; ABTasty=uid=s43fcyc0hr2z8nkz&fst=1643568412317&pst=1643568412317&cst=1643650069991&ns=2&pvt=35&pvis=13&th=; ABTastySession=mrasn=&sen=12&lp=https%253A%252F%252Fwww.idealista.com%252F; utag_main=v_id:017eac27920f001f94d41f93932f05072002f06a00978$_sn:3$_se:17$_ss:0$_st:1643653573939$dc_visit:2$ses_id:1643650066499%3Bexp-session$_pn:13%3Bexp-session$_prevVtSource:directTraffic%3Bexp-1643653668218$_prevVtCampaignCode:%3Bexp-1643653668218$_prevVtDomainReferrer:%3Bexp-1643653668218$_prevVtSubdomaninReferrer:%3Bexp-1643653668218$_prevVtUrlReferrer:%3Bexp-1643653668218$_prevVtCampaignLinkName:%3Bexp-1643653668218$_prevVtCampaignName:%3Bexp-1643653668218$_prevVtRecommendationId:%3Bexp-1643653668218$_prevCompletePageName:12%3A%3Adetail%3A%3A%3A%3A%3A%3Ahome%3Bexp-1643655375478$_prevLevel2:12%3Bexp-1643655375478$_prevAdId:94476328%3Bexp-1643655375489$_prevAdOriginTypeRecommended:undefined%3Bexp-1643653668228$dc_event:5%3Bexp-session$dc_region:us-east-1%3Bexp-session; cto_bundle=5izo9l9XUkgzNEJIYU9ITGFNWU5xMWp4YWoxNCUyRnFJN2dHbFJTbWRCVlpldHRFbjF4aFFaSmFvWHZvV3B2WCUyRmx1MEhiMUNQbDlUQmlPckkwdW51VTdTZjltc255c0ZjRng0d1c4MXVFVURBdUFXdEYwaSUyQnhtSlowY3oybzE4TkxtY2RVWmR3ZHJvbVJvUko1MmQxT1FJczJvV2clM0QlM0Q; datadome=v475FOZILU7oISIcQM..EhoU-BUZiH-ZFK2rcgXnvpb4HM05afYuAyFXtJCwx2Wz7bUmxBeuhegZikjdshl3ZtuSvzBOApWI_EnGCatwDUdNRGdY_Ox0oK6H5CgXTyT''',
"pragma": "no-cache",
"referer": "https://www.idealista.com/inmueble/95249555/",
"sec-ch-ua": '" Not;A Brand";v="99", "Google Chrome";v="97", "Chromium";v="97"',
"sec-ch-ua-mobile": "?0",
"sec-ch-ua-platform": '"Windows"',
"sec-fetch-dest": "document",
"sec-fetch-mode": "navigate",
"sec-fetch-site": "same-origin",
"sec-fetch-user": "?1",
"upgrade-insecure-requests": "1",
"user-agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/97.0.4692.99 Safari/537.36"
    
}


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


# In[6]:


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
    
    regex_m2 = re.compile(r'\d\d\d m²|\d\d m²')
    regex_hab = re.compile(r'\d hab|sin habitación')
    regex_baño = re.compile(r'\d baño')
    regex_planta = re.compile(r'planta \d\d|planta \d|bajo')
    
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
                
                time.sleep(random.randint(3,4)*random.random())
        
            except Exception as e:
                print(f"error {e}")
        
#                 dic[i] = ["error"] * 50
                          
    return dic

# test


# In[7]:


def dic_to_df(dic):
    '''
    Transform dic from Idealista https://www.idealista.com/inmueble
    to a data frame.
    
    dic -> Dataframe
    '''
    colnames = ['name','zone','ubicacion_full','distrito2','calle','barrio','barrio2','distrito','area','price','price_before','square_mt','rooms','wc','terraza','balcon','estado',     'año','armarios','cocina','amueblado','planta','calef','asc','aire','exterior','datalles2','cp','actualizacion','actualizacion2','extract_day']
    df = pd.DataFrame.from_dict(dic, orient='index',columns=colnames)
    
    return df
    


# In[8]:


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
        df['planta'] = df.planta.apply(lambda x: '0' if x == "" else x)
        df['planta'] = df.planta.apply(lambda x: '0' if x == "bajo" else x)
        df['planta'] = df.planta.astype(int)
    except:
        pass
    df['cp'] = df.cp.astype(str)
    
    return df


# #### Lista de codigos postales

# In[2]:





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
        


# In[14]:


def chunk(lst, n):
    """Yield successive n-sized chunks from lst."""
    for i in range(0, len(lst), n):
        yield lst[i:i + n]
        


# In[15]:


def call_scrapper(cp_list,parent_path): # poner headers aqui
    
    print("parent directory: ",os.getcwd())
    
    new_dir_name = create_folder(parent_path)
    
    os.chdir("{}\\{}".format(parent_path,new_dir_name))

    print("csv folder: ",os.getcwd())
    
    for chunked_list in chunk(cp_list, 6):
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

        time.sleep(random.randint(750,900))


# In[26]:


def concat_csv(parent_path, folder=None):
    
    colnames = ['id','name','zone','ubicacion_full','calle','barrio','barrio2','distrito','area','price','price_before','square_mt','rooms','wc','terraza','balcon','estado',     'año','armarios','cocina','amueblado','planta','calef','asc','aire','exterior','datalles2','cp','actualizacion','actualizacion2','extract_day']
    df_final = pd.DataFrame(columns = colnames)
    
    if folder is None:
        day_save = date.today()
        directory = "extraction_{}".format(today)
        path = os.path.join(parent_path, directory)
        try:
            os.chdir(path)
            print("path exists")
            csv = os.listdir()
        except ValueError:
            print("path does not exists")

        
    
    if folder is not None:
        day_save = re.search('\d{4}-\d{2}-\d{2}', folder)[0]
        path = os.path.join(parent_path, folder)
        try:
            os.chdir(path)
            print("path exists")
            csv = os.listdir()
        except ValueError:
            print("path does not exists")
            
        
        
    for i in csv:
        df = pd.read_csv(i)
        df_final = pd.concat([df_final,df],axis=0,ignore_index=True)
        
    df_final.to_csv("datos_scrapping_{}.csv".format(day_save),encoding = 'utf-8-sig',index=False)
    print("file '% s' created" % "datos_scrapping_{}.csv".format(day_save))
        
    


