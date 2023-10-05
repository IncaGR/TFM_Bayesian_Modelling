from Idealista_scrapping.utils import *
import yaml

with open('.\\Desktop\\1_projects\\TFM\\2_code\\1_extraction\\secrets.yaml', 'r') as secrets_file:
    secrets = yaml.safe_load(secrets_file)
data_idealista_path = secrets['path']['data_idealista_path']
print(data_idealista_path)

# TODO: put the paths in yaml file
# data_idealista_path = '.\\Desktop\\1_projects\\TFM\\1_data\\2_data_Idealista\\1_raw'

# TODO: use the last extraction folder
concat_csv(data_idealista_path,"extraction_2023-10-04") # using today to concat all the csv files