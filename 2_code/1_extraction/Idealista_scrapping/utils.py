from datetime import date
import re
import pandas as pd
import os



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