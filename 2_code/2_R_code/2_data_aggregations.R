library(tidyverse)
library(here)
library(car)

getwd()

wd = "C:/Users/ggari/Desktop/Master_MESIO/TFM"

setwd(wd)

path_functions = here(wd,"srapper","functions_R","clean_data.R")

source(path_functions)


# unique(data_idealista$distrito2)

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

path_idealista = here(wd,"srapper","data_scrapping","idealista_data_clean_2.csv") # already cleaned

path_shp_barcelona = here(wd,"Barrios_de_Barcelona","0301040100_Barris_ADM_ETRS89.shp")

data_idealista = read.csv(path_idealista,encoding = "UTF-8")

barcelona_shape <- sf::st_read(path_shp_barcelona)


mapping = mapIdBarri(barcelona_shape)

mapping = regex_nom_barris(mapping,mapping$barri)


unique(data_idealista$key_shp)

unique(data_idealista$key_open)


data_idealista = left_join(data_idealista,mapping, by=c("key_open" ="regex_barris"))

data_idealista = left_join(data_idealista,table_districte, by="distrito2")


summary(data_idealista)


data_idealista = data_idealista %>% dplyr::filter(!is.na(codi_districte))

data_idealista$id_barri <- as.numeric(data_idealista$id_barri)

summary(data_idealista)


unique((data_idealista %>% dplyr::filter(is.na(id_barri)))$regex_barri)


data_idealista = data_idealista %>% dplyr::filter(!is.na(id_barri))


ggplot(data_idealista,aes(square_mt,price, col = distrito2)) + 
  geom_jitter()


summary(data_idealista)
# ggplot(data_idealista,aes(square_mt,price, col = barri)) + 
#   geom_jitter()

# # clean some names
# 
# data_idealista = data_idealista %>%
#   dplyr::filter(!distrito2 %in% c("Teatinos","Playa de Palma","La Torrasa","Santa Eulàlia"))
# 
# # applying the normalization names function
# data_idealista = regex_nom_barris(data_idealista,data_idealista$barrio2)
# barcelona_shape = regex_nom_barris(barcelona_shape,barcelona_shape$NOM)
# 
# 
# # changing some names in Idealista data
# data_idealista = data_idealista %>%
#   mutate(key_open = regex_barris,
#          # shape_nom_barri = ifelse(barrio_lower == "el gòtic","el barri gòtic",shape_nom_barri),
#          # shape_nom_barri = ifelse(barrio_lower == "sant pere - santa caterina i la ribera",
#          #                          "sant pere - santa caterina i la ribera",shape_nom_barri),
#          # shape_nom_barri = ifelse(barrio_lower == "el poble sec - parc de montjuïc",
#          #                          "el poble-sec", shape_nom_barri),
#          # shape_nom_barri = ifelse(barrio_lower == "vila de gràcia", "la vila de gràcia",shape_nom_barri),
#          # shape_nom_barri = ifelse(barrio_lower == "la marina del port", "la marina del port", shape_nom_barri),
#          # shape_nom_barri = ifelse(barrio_lower == "vallvidrera - el tibidabo i les planes",
#          #                          "vallvidrera, el tibidabo i les planes", shape_nom_barri),
#          # shape_nom_barri = ifelse(barrio_lower == "el besòs", "el besòs i el maresme", shape_nom_barri),
#          key_open = ifelse(regex_barris == "besòs", "besòs maresme", key_open),
#          
#          key_open = ifelse(regex_barris == "can peguera turó peira", "can peguera",
#                              key_open), # recuerda que idealista estan juntos, shp dos barrios distintos
#          key_open = ifelse(regex_barris == "ciutat meridiana torre baró vallbona",
#                              "ciutat meridiana", key_open),
#          key_open = ifelse(regex_barris ==  "sant genís agudells montbau",
#                              "montbau", key_open),
#          key_open = ifelse(regex_barris == "vall hebron clota", "vall hebron", key_open),
#          key_open = ifelse(regex_barris == "poble sec parc montjuïc", "poble sec", key_open)
#   )

# changing some names in shp data
# barcelona_shape = barcelona_shape %>%
#   mutate(key_barris = regex_barris,
#          # idealista_nom_barri = ifelse(nom_lower == "el barri gòtic", "el gòtic",
#          #                              idealista_nom_barri),
#          # idealista_nom_barri = ifelse(nom_lower == "sant pere, santa caterina i la ribera",
#          #                              "sant pere - santa caterina i la ribera", idealista_nom_barri),
#          key_barris = ifelse(regex_barris == "poble sec", "poble sec parc montjuïc",
#                              key_barris),
#          # idealista_nom_barri = ifelse(nom_lower == "la marina de port", "la marina del port",
#          # idealista_nom_barri),
#          # idealista_nom_barri = ifelse(nom_lower == "la vila de gràcia", "vila de gràcia",
#          # idealista_nom_barri),
#          key_barris = ifelse(regex_barris == "sant genís agudells", "sant genís agudells montbau",
#                              key_barris),
#          key_barris = ifelse(regex_barris == "montbau", "sant genís agudells montbau", key_barris),
#          key_barris = ifelse(regex_barris == "vall hebron","vall hebron clota",
#                              key_barris),
#          key_barris = ifelse(regex_barris == "clota", "vall hebron clota",
#                              key_barris),
#          key_barris = ifelse(regex_barris == "turó peira", "can peguera turó peira",
#                              key_barris),
#          # idealista_nom_barri = ifelse(nom_lower == "vallvidrera, el tibidabo i les planes",
#          #                              "vallvidrera, el tibidabo i les planes", idealista_nom_barri),
#          key_barris = ifelse(regex_barris == "besòs maresme", "besòs", key_barris),
#          key_barris = ifelse(regex_barris == "can peguera", "can peguera turó peira",
#                              key_barris),
#          # idealista_nom_barri = ifelse(nom_lower == "la trinitat nova", "")) no encuentro
#          key_barris = ifelse(regex_barris == "torre baró", "ciutat meridiana torre baró vallbona",
#                              key_barris),
#          key_barris = ifelse(regex_barris == "ciutat meridiana", "ciutat meridiana torre baró vallbona",
#                              key_barris),
#          key_barris = ifelse(regex_barris == "vallbona","ciutat meridiana torre baró vallbona",
#                              key_barris)
#          # ,idealista_nom_barri = ifelse(nom_lower == "baró de viver") no encuentro
#   )


# 
# idx = sort(unique(data_idealista$regex_barris)) %in% sort(unique(barcelona_shape$key_barris))
# 
# sort(unique(data_idealista$key_barris))[!idx]


# to connect Idealista data and shp file the key is: key_shp
# data_idealista = data_idealista %>%
#   mutate(key_shp = regex_barris)

# barcelona_shape = barcelona_shape %>%
#   mutate(key_shp = key_barris)

# idx = sort(unique(data_idealista$key_barris)) %in% sort(unique(barcelona_shape$regex_barris))
# 
# sort(unique(data_idealista$key_barris))[!idx]

# write_csv2(data_idealista,"C:/Users/ggari/Desktop/Master_MESIO/TFM/srapper/data_scrapping/idealista_data_clean.csv")

# # data cleaning -----------------------------------------------------------
# 
# 
# ggplot(data_idealista,aes(square_mt,price)) + 
#   geom_jitter()
# 
# # let's check the observation with price around 80000/ month.
# 
# # data_idealista[data_idealista$price>70000,] # vamos a remover la observacion
# # 
# # 
# # # let's check the observations with more than 250 square meters:
# # 
# # big_mt2 = data_idealista[data_idealista$square_mt> 250,] 
# 
# # filtrar piso o chalet:
# # loft con 1/0 rooms
# # wc > 4?
# # alquiler de temporada
# 
# # colnames(data_idealista)
# 
# # data_idealista = data_idealista %>%
# #   rename(nombre = X.U.FEFF.name)
# 
# data_idealista = data_idealista[data_idealista$price<70000,]
# 
# data_idealista = data_idealista[data_idealista$square_mt>10,] # casa con 0 metros, eliminada
# 
# 
# ## Adding new binary variables
# data_idealista = data_idealista %>%
#   mutate(casa = ifelse(grepl("Casa o chalet",data_idealista$nombre)==TRUE,1,0))
# 
# data_idealista = data_idealista %>%
#   mutate(estudio = ifelse(grepl("Estudio",data_idealista$nombre)==TRUE,1,0))
# 
# ## Convert to factor some variables
# 
# # unique(pisos_2$wc)
# 
# data_idealista = data_idealista %>%
#   dplyr::mutate(wc2 = ifelse(wc >= 3, "3 or more", wc),
#                 wc2 = ifelse(wc == 2, "2", wc2),
#                 wc2 = ifelse(wc == 1, "1", wc2)
#   )
# 
# # unique(pisos_2$wc2)
# 
# data_idealista$wc2 <- factor(x = data_idealista$wc2, levels = c("1","2","3 or more"))
# 
# # str(pisos_2$wc2)
# 
# 
# ggplot(data_idealista, aes(log(price))) +
#   geom_histogram(bins = 50)
# 
# ggplot(data_idealista, aes(log(square_mt))) +
#   geom_histogram(bins = 50)
# 
# data_idealista = data_idealista%>%
#   dplyr::mutate(rooms2 = ifelse(rooms >= 4, "4 or more", rooms),
#                 rooms2 = ifelse(rooms == 3, "3", rooms2),
#                 rooms2 = ifelse(rooms == 2, "2", rooms2),
#                 rooms2 = ifelse(rooms  <= 1, "1", rooms2)
#   )
# 
# 
# data_idealista$rooms2 <- factor(x = data_idealista$rooms2, levels = c("1","2","3","4 or more"))
# data_idealista$log_price <- log(data_idealista$price)
# 
# ## merging terraza and blacon
# 
# data_idealista = data_idealista %>%
#   mutate(terraza_balcon = ifelse((data_idealista$terraza == 1)|(data_idealista$balcon == 1),1,0))
# 
# 
# # check
# 
# ggplot(data_idealista,aes(square_mt,price)) + 
#   geom_jitter()


# function read data API --------------------------------------------------

# setwd('C:/Users/ggari/Desktop/Master_MESIO/TFM/srapper/dataset_opendata')
# setwd(here(wd,"srapper","dataset_opendata"))
files <- list.files(here(wd,"srapper","dataset_opendata"))
# getwd()

read_API_data()

# hospitales --------------------------------------------------------------


# hospitales = regex_nom_barris(hospitales,hospitales$addresses_neighborhood_name)

# head(hospitales)
# colnames(hospitales)
# 
# unique(hospitales$secondary_filters_name)
# sort(unique(hospitales$addresses_neighborhood_name))

# summary(hospitales)

# hospitales$regex_barris

# basic aggregation function, number per neighbors and district
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


# ya podria agregar esto al dataset original.

n_hospital_barri = hospitales %>%
  group_by(addresses_neighborhood_id,addresses_neighborhood_name,secondary_filters_name) %>%
  count(name = "counts",sort = T)

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

# n_all_barris = hospitales %>%
#   group_by(addresses_neighborhood_id) %>%
#   count(name = "total_hospitals",sort = T)


# perform the join with wider_type an n_all_barris

# data_idealista$id_barri = as.integer(data_idealista$id_barri)
# 
data_idealista = left_join(data_idealista,wider_type,by=c("id_barri" = "addresses_neighborhood_id"))
# data_idealista = left_join(data_idealista,n_all_barris,by=c("id_barri" = "addresses_neighborhood_id"))


summary(data_idealista)

# data_idealista[]

# 
# data_idealista[is.na(data_idealista$hospitals),]
# 
# unique(data_idealista[is.na(data_idealista$hospitals),]$barri)
# 
# unique(hospitales$addresses_neighborhood_name)

##### NO HAY CAPS, CUAPS, HOSPITALES EN LOS SIGUIENTRES BARRIOS:

# [1] "la Font de la Guatlla"             "el Parc i la Llacuna del Poblenou"
# [3] "Hostafrancs"                       "Porta"                            
# [5] "el Coll"                           "Can Baró"                         
# [7] "la Teixonera"                      "el Congrés i els Indians"         
# [9] "Navas"                             "Can Peguera"                      
# [11] "la Font d'en Fargues"              "la Vall d'Hebron"                 
# [13] "Ciutat Meridiana"                  "Pedralbes"                        
# [15] "la Marina del Prat Vermell"   

# view = data_idealista[is.na(data_idealista$hospitals),]
# All good the places with NA have not hospitals

# Fill NA's
# data_idealista = data_idealista %>%
#   replace_na(list("hospitals" = 0,
#                   "caps" = 0,
#                   "cuaps" = 0,
#                   "total_hospitals" = 0))


# view = data_idealista %>%
#   dplyr::filter(is.na(hospitals))


# unique(hospitales$addresses_neighborhood_name)



# por distritos tmb?
# No needed 

# n_hospital_districte = hospitales %>%
#   group_by(addresses_district_name,secondary_filters_name) %>%
#   count(name = "counts",sort = T)
# 
# 
# ratio_hosp_d = hospitales %>%
#   group_by(addresses_district_name) %>%
#   summarise(count = n(), ratio_hosp_d = n()/nrow(hospitales))
# 
# wider_type_d = n_hospital_districte %>%
#   pivot_wider(names_from = secondary_filters_name,values_from = counts,values_fill = 0)
# 
# colnames(wider_type_d)
# 
# wider_type_d = wider_type_d %>%
#   rename(hospitals_d = "Hospitals i clíniques",
#          caps_d = "CAPs"  ,
#          cuaps_d = "Centres urgències (CUAPs)")
# 
# n_all_districtes = hospitales %>%
#   group_by(addresses_district_name) %>%
#   count(name = "total_hospitals_districte",sort = T)
# 
# 
# ratio_hosp_d = hospitales %>%
#   group_by(addresses_district_name) %>%
#   summarise(count = n(), ratio_hosp_d = n()/nrow(hospitales))
# 
# 
# # perform the join with wider_type_d an n_all_districte
# 
# data_idealista = left_join(data_idealista,wider_type_d,by= c("distrito2" = "addresses_district_name"))
# data_idealista = left_join(data_idealista,ratio_hosp_d,by=c("distrito2" = "addresses_district_name"))
# 
# summary(data_idealista)


# terrasas ----------------------------------------------------------------

colnames(terrazas2)
head(terrazas2)


data_idealista = groupingDataOpenData(data_idealista,terrazas2,"terrazas")

groupingDataOpenData(data_idealista,terrazas2,"terrazas")


summary(data_idealista)


unique(data_idealista[is.na(data_idealista$n_terrazas_barri),]$barri)

# # agrupacion numero de terrazas
# n_terrazas_b = terrazas2 %>%
#   group_by(nom_barri,codi_barri)%>%
#   count(name = "n_terrazas_barri", sort = T)
# 
# n_terrazas_d = terrazas2 %>%
#   group_by(nom_districte)%>%
#   count(name = "n_terrazas_districte", sort = T)

# mean_terrazas = terrazas2%>%
#   group_by(nom_barri,codi_barri)%>%
#   summarise(mean_cadires_b = mean(cadires),mean_superficie_b=mean(superficie_ocupada),
#             mean_taules_b = mean(taules))


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

# Fill all NA with 0 (know that are from the barris/ districtes aggreggation)
data_idealista <- data_idealista %>% replace(is.na(.), 0)

summary(data_idealista)

# groupingDataOpenData <-function(df_idealista,df_open_data, name =NA){

# 
#   if("codi_barri" %in% colnames(df_open_data)) {
#     print("codi_barri")
#     
#     # name_col = cat("n_",name,"_barri")
#     
#     groupData = df_open_data %>%
#       group_by(codi_barri) %>%
#       count(name = paste0("n_",name,"_barri",sep = ""), sort = T)
#     
#     print(groupData)
#     
#     groupDataDis = df_open_data %>%
#       group_by(nom_districte) %>%
#       count(name = paste0("n_",name,"_districte",sep = ""), sort = T)
#     
#     
#     df_idealista_final = left_join(df_idealista,groupData,by = c("id_barri"="codi_barri"))
#     df_idealista_final = left_join(df_idealista_final,groupDataDis,by = c("distrito2"="nom_districte"))
#     
#     return(df_idealista_final)
#       
#   }
#   
#   if("addresses_neighborhood_id" %in% colnames(df_open_data)){
#     
#     print("addresses_neighborhood_id")
#   }
# }



# terrazasGrouped = groupingDataOpenData(data_idealista,terrazas2,"terrazas")



# unique(terrazas2$nom_barri)
# unique(terrazas2$regex_barris)
# 
# unique(data_idealista$regex_barris)
# 
# unique(data_idealista$barrio2)
# ading binaris type loft and casa o chalet


# Si uso barris, me da perfecta colinearidad
# si uso distritos y agregaciones de num hospitales por distrito tmb..
# solo caracteristicas intrinsecas del piso 0.7105 y ajustado 0.7056 + barrios

# library(car)

# Dataset añadidos
# Hospitals
# Terrazas




