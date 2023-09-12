library(tidyverse)
library(here)
library(car)

getwd()

# date_of_data = "2023-05-03" # put the date of the file you want to aggregate
date_of_data = "2023-06-05" # test sample
date_of_data = "2023-07-09" # test sample
date_of_data = "2023-08-03" # test sample

# date_of_data = "2023-04-20" # put the date of the file you want to clean

path_functions = here("2_code","2_R_code","functions","data_mapping.R")

source(path_functions)


# dummy table to map the data between Idealista and data from API Barcelona

table_districte = data.frame(codi_districte = c(1:10),
                             distrito2= c("Distrito Ciutat Vella",
                                          "Distrito Eixample",
                                          "Distrito Sants-Montjuïc",
                                          "Distrito Les Corts",
                                          "Distrito Sarrià-Sant Gervasi",
                                          "Distrito Gràcia",
                                          "Distrito Horta Guinardó",
                                          "Distrito Nou Barris",
                                          "Distrito Sant Andreu",
                                          "Distrito Sant Martí"))






# Reading data ------------------------------------------------------------
path_data_Ide = "1_data/2_data_Idealista"

path_csv_clean = paste0("data_idealista_clean_",date_of_data,".csv")

path_idealista = here(path_data_Ide,"2_clean",path_csv_clean)

path_shp_barcelona = here("1_data","3_data_Barris_Barcelona","0301040100_Barris_ADM_ETRS89.shp")

data_idealista = read.csv(path_idealista,encoding = "UTF-8")

barcelona_shape <- sf::st_read(path_shp_barcelona)


# Mapping data  -----------------------------------------------------------

mapping = mapIdBarri(barcelona_shape)

mapping = regex_nom_barris(mapping,mapping$barri)


unique(data_idealista$key_shp)

unique(data_idealista$key_open)

# mapping by id because mapping by names cause errors, different names.. etc
data_idealista = left_join(data_idealista,mapping, by=c("key_open" ="regex_barris"))

data_idealista = left_join(data_idealista,table_districte, by="distrito2")


summary(data_idealista)

# filtering data if the id is not present:
# sometime the scrapper return data from outside of Barcelona
data_idealista = data_idealista %>% dplyr::filter(!is.na(codi_districte))

data_idealista$id_barri <- as.numeric(data_idealista$id_barri)

summary(data_idealista)


unique((data_idealista %>% dplyr::filter(is.na(id_barri)))$regex_barri)


data_idealista = data_idealista %>% dplyr::filter(!is.na(id_barri))

summary(data_idealista)

ggplot(data_idealista,aes(square_mt,price, col = distrito2)) + 
  geom_jitter()

# numero de barrios
unique(data_idealista$id_barri)

# reading data API Barcelona ----------------------------------------------

path_open_data <- "1_data/1_data_API/dataset_opendata"


files <- list.files(here(path_open_data))
# getwd()

read_API_data()


# Aggregations data API Barcelona -----------------------------------------


# Hospitals ---------------------------------------------------------------

data_idealista = groupingDataOpenData(data_idealista,hospitales,"hospitals")

# see how many centres de dia we have

# hospitales %>%
#   group_by(secondary_filters_name) %>%
#   count(name = "counts", sort = T)

# too litle "centres de dia" , "Residencies gent gran". Let's remove

hospitales = hospitales %>%
  dplyr::filter(secondary_filters_name %in% c("Hospitals i clíniques","CAPs","Centres urgències (CUAPs)"))

# hospitales %>%
#   dplyr::filter(!secondary_filters_name %in% c("Hospitals i clíniques","CAPs","Centres urgències (CUAPs)"))

n_hospital_barri = hospitales %>%
  group_by(addresses_neighborhood_id,addresses_neighborhood_name,secondary_filters_name) %>%
  count(name = "counts",sort = T)

print(n_hospital_barri)

# codi_barri
# addresses_neighborhood_id

# do pivot and have 3 columns or have everything together?
# colnames(n_hospital_barri)

wider_type = n_hospital_barri %>%
  pivot_wider(names_from = secondary_filters_name,values_from = counts,values_fill = 0)

# colnames(wider_type)

wider_type = wider_type %>%
  rename(hospitals = "Hospitals i clíniques",
         caps = "CAPs"  ,
         cuaps = "Centres urgències (CUAPs)")

print(wider_type)

# n_all_barris = hospitales %>%
#   group_by(addresses_neighborhood_id) %>%
#   count(name = "total_hospitals",sort = T)


# perform the join with wider_type an n_all_barris

# data_idealista$id_barri = as.integer(data_idealista$id_barri)
# 
data_idealista = left_join(data_idealista,wider_type,by=c("id_barri" = "addresses_neighborhood_id"))
# data_idealista = left_join(data_idealista,n_all_barris,by=c("id_barri" = "addresses_neighborhood_id"))


# There are 365 NA's when merging the hospital data with the Idealista dataset
# let's see if that is because there are no equipment in zone neighbourhoods
# or there are an error doing the maapping
summary(data_idealista)

dim(data_idealista)

# to check the difference of length between the Idealista missing data for hospitals
# and the id from hospitals data that have not equipment
list_idealista = data_idealista %>% dplyr::filter(is.na(caps)) %>% pull(id_barri) %>% unique()

list_hospital = wider_type %>% ungroup() %>% pull(addresses_neighborhood_id) 

diff = base::setdiff(list_idealista,list_hospital)

if(length(list_idealista) == length(diff)) {
  
  
  print("The NA's are correct as those id neigbourhoods have not equipment for hospitals data.")
} else{
  print("There are errors. Review the data.")
}



# Terrasas ----------------------------------------------------------------

colnames(terrazas2)
head(terrazas2)


data_idealista = groupingDataOpenData(data_idealista,terrazas2,"terrazas")

groupingDataOpenData(data_idealista,terrazas2,"terrazas")


summary(data_idealista)


unique(data_idealista[is.na(data_idealista$n_terrazas_barri),]$barri)




rev = terrazas2%>%
  group_by(nom_barri,codi_barri)%>%
  summarise(mean_cadires_b = mean(cadires),mean_superficie_b=mean(superficie_ocupada),
            mean_taules_b = mean(taules)) %>%
  arrange(codi_barri)

rev

# 
# data_idealista = left_join(data_idealista,n_terrazas_b,by=c("id_barri" = "codi_barri"))
# data_idealista = left_join(data_idealista,n_terrazas_d,by=c("distrito2" = "nom_districte"))
# 
data_idealista = left_join(data_idealista,rev,by=c("id_barri" = "codi_barri"))

summary(data_idealista)


# view = data_idealista[is.na(data_idealista$caps),]




# arbres ------------------------------------------------------------------

colnames(arbres_bcn)

# unique(arbres_bcn$espai_verd)

data_idealista = groupingDataOpenData(data_idealista,arbres_bcn,"arbres_bcn")

summary(data_idealista)


data_idealista = groupingDataOpenData(data_idealista,arbres_viaris,"arbres_viaris")

summary(data_idealista)


# bares -------------------------------------------------------------------

unique(bar_copas$secondary_filters_name)

bar_copas %>%
  group_by(secondary_filters_name)%>%
  count() %>%
  arrange(-n)


dim(bar_copas)

length(unique(bar_copas$name))

data_idealista = groupingDataOpenData(data_idealista,bar_copas,"bar_copas")

summary(data_idealista)



# net income per house ----------------------------------------------------
renda_neta = renda_neta_mitjana_per_llar_20

mean_renda_neta = renda_neta %>% group_by(codi_barri) %>% summarise(mean_income=mean(import_euros))
names(mean_renda_neta)
names(data_idealista)

data_idealista = left_join(data_idealista,mean_renda_neta, by =c('id_barri' = 'codi_barri'))


# Fill all NA with 0 (know that are from the barris/ districtes aggreggation)
data_idealista <- data_idealista %>% replace(is.na(.), 0)

summary(data_idealista)


# atur perct --------------------------------------------------------------

names(atur_23_perc)
atur_grp = atur_23_perc %>% group_by(any,codi_barri) %>% summarise(mean_perc_atur = mean(pes_atur))
data_idealista = left_join(data_idealista,atur_grp, by =c('id_barri' = 'codi_barri'))
# Save document

date_to_save <- str_extract(path_idealista, "\\d{4}-\\d{2}-\\d{2}")

path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/data_modelling_",date_to_save,".RDS")

path_to_save_csv = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/data_modelling_",date_to_save,".csv")

saveRDS(data_idealista,file=path_to_save)

readr::write_csv(data_idealista,path_to_save_csv)

print(path_to_save)

# table(data_idealista$wc2)
# table(data_idealista$wc)
