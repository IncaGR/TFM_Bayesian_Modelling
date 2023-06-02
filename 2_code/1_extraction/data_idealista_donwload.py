
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

from scrapping_function.scrapping_functions import *

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


cp = ["{:05d}".format(i) for i in range(8001,8043)] # 8001 - 8010 , 8011 - 8021, 8021 - 8031
cp

url_cp = "https://www.idealista.com/buscar/alquiler-viviendas/{}/".format(cp[0])

parent_path = 'C:\\Users\\ggari\\Desktop\\1_projects\\TFM'

data_idealista_path = 'C:\\Users\\ggari\\Desktop\\1_projects\\TFM\\1_data\\2_data_Idealista\\1_raw'


call_scrapper(cp,data_idealista_path)

concat_csv(data_idealista_path,folder="extraction_2023-05-30")
