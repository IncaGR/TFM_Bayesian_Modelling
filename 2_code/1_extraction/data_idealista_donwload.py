
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

from Idealista_scrapping.scrapping_functions import *

with open('.\\Desktop\\1_projects\\TFM\\2_code\\1_extraction\\secrets.yaml', 'r') as secrets_file:
    secrets = yaml.safe_load(secrets_file)
headers = secrets['headers']

cp = ["{:05d}".format(i) for i in range(8005,8043)] # 8001 - 8010 , 8011 - 8021, 8021 - 8031
# cp

# url_cp = "https://www.idealista.com/buscar/alquiler-viviendas/{}/".format(cp[0])

# TODO: put the paths in yaml file
# parent_path = '.\\Desktop\\1_projects\\TFM'

# data_idealista_path = '.\\Desktop\\1_projects\\TFM\\1_data\\2_data_Idealista\\1_raw'

parent_path = secrets['path']['parent_path']
data_idealista_path = secrets['path']['data_idealista_path']


call_scrapper(cp,data_idealista_path)

# concat_csv(data_idealista_path,"extraction_2023-08-03")