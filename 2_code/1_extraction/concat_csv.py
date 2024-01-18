from pyprojroot.here import here
import yaml

from Idealista_scrapping.utils import *


print(here())

with open('C:\\Users\\galag\\Desktop\\1_projects\\TFM\\2_code\\1_extraction\\secrets.yaml', 'r') as secrets_file:
    secrets = yaml.safe_load(secrets_file)
data_idealista_path = secrets['path']['data_idealista_path']
print(data_idealista_path)

# TODO: use the last extraction folder
concat_csv(data_idealista_path,"extraction_2023-10-29") # using today to concat all the csv files