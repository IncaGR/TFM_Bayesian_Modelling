library(tidyverse)
library(here)
library(car)

getwd()

wd = "C:/Users/ggari/Desktop/Master_MESIO/TFM"

path_functions = here(wd,"srapper","functions_R","clean_data.R")

source(path_functions)


# Reading data ------------------------------------------------------------

path_idealista = here(wd,"srapper","extraction_2022-11-01","datos_scrapping_2022-11-01.csv")
path_idealista = here(wd,"srapper","extraction_2022-11-17","datos_scrapping_2022-11-19.csv")

path_shp_barcelona = here(wd,"Barrios_de_Barcelona","0301040100_Barris_ADM_ETRS89.shp")

data_idealista = read.csv(path_idealista,encoding = "UTF-8")

barcelona_shape <- sf::st_read(path_shp_barcelona)


# clean some names

data_idealista = data_idealista %>%
  dplyr::filter(!distrito2 %in% c("Teatinos","Playa de Palma","La Torrasa","Santa Eulàlia"))

# applying the normalization names function
data_idealista = regex_nom_barris(data_idealista,data_idealista$barrio2)
barcelona_shape = regex_nom_barris(barcelona_shape,barcelona_shape$NOM)


# changing some names in Idealista data
data_idealista = data_idealista %>%
  mutate(key_open = regex_barris,
         # shape_nom_barri = ifelse(barrio_lower == "el gòtic","el barri gòtic",shape_nom_barri),
         # shape_nom_barri = ifelse(barrio_lower == "sant pere - santa caterina i la ribera",
         #                          "sant pere - santa caterina i la ribera",shape_nom_barri),
         # shape_nom_barri = ifelse(barrio_lower == "el poble sec - parc de montjuïc",
         #                          "el poble-sec", shape_nom_barri),
         # shape_nom_barri = ifelse(barrio_lower == "vila de gràcia", "la vila de gràcia",shape_nom_barri),
         # shape_nom_barri = ifelse(barrio_lower == "la marina del port", "la marina del port", shape_nom_barri),
         # shape_nom_barri = ifelse(barrio_lower == "vallvidrera - el tibidabo i les planes",
         #                          "vallvidrera, el tibidabo i les planes", shape_nom_barri),
         # shape_nom_barri = ifelse(barrio_lower == "el besòs", "el besòs i el maresme", shape_nom_barri),
         key_open = ifelse(regex_barris == "besòs", "besòs maresme", key_open),
         
         key_open = ifelse(regex_barris == "can peguera turó peira", "can peguera",
                           key_open), # recuerda que idealista estan juntos, shp dos barrios distintos
         key_open = ifelse(regex_barris == "ciutat meridiana torre baró vallbona",
                           "ciutat meridiana", key_open),
         key_open = ifelse(regex_barris ==  "sant genís agudells montbau",
                           "montbau", key_open),
         key_open = ifelse(regex_barris == "vall hebron clota", "vall hebron", key_open),
         key_open = ifelse(regex_barris == "poble sec parc montjuïc", "poble sec", key_open)
  )

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

# 
# idx = sort(unique(data_idealista$regex_barris)) %in% sort(unique(barcelona_shape$key_barris))
# 
# sort(unique(data_idealista$key_barris))[!idx]


# to connect Idealista data and shp file the key is: key_shp
data_idealista = data_idealista %>%
  mutate(key_shp = regex_barris)

# barcelona_shape = barcelona_shape %>%
#   mutate(key_shp = key_barris)

# idx = sort(unique(data_idealista$key_barris)) %in% sort(unique(barcelona_shape$regex_barris))
# 
# sort(unique(data_idealista$key_barris))[!idx]

data_idealista = data_idealista %>%
  dplyr::rename(id = X.U.FEFF.id)


# write_csv2(barcelona_shape,"C:/Users/ggari/Desktop/Master_MESIO/TFM/Barrios_de_Barcelona/barrios_bcn_clean.csv")


# data cleaning -----------------------------------------------------------


ggplot(data_idealista,aes(square_mt,price)) + 
  geom_jitter()

# let's check the observation with price around 80000/ month.

# data_idealista[data_idealista$price>70000,] # vamos a remover la observacion
# 
# 
# # let's check the observations with more than 250 square meters:
# 
# big_mt2 = data_idealista[data_idealista$square_mt> 250,] 

# filtrar piso o chalet:
# loft con 1/0 rooms
# wc > 4?
# alquiler de temporada

# colnames(data_idealista)

# data_idealista = data_idealista %>%
#   rename(nombre = X.U.FEFF.name)

# data_idealista = data_idealista[data_idealista$price<70000,]
# data_idealista = data_idealista[data_idealista$price<10000,] # testing 
data_idealista = data_idealista[data_idealista$price<7500,] # testing 

data_idealista = data_idealista[data_idealista$square_mt>10,] # casa con 0 metros, eliminada
data_idealista = data_idealista[data_idealista$square_mt<375,]

## Adding new binary variables
data_idealista = data_idealista %>%
  mutate(casa = ifelse(grepl("Casa o chalet",data_idealista$name)==TRUE,1,0))

data_idealista = data_idealista %>%
  mutate(estudio = ifelse(grepl("Estudio",data_idealista$name)==TRUE,1,0))

## Convert to factor some variables

# unique(pisos_2$wc)

data_idealista = data_idealista %>%
  dplyr::mutate(wc2 = ifelse(wc >= 3, "3 or more", wc),
                wc2 = ifelse(wc == 2, "2", wc2),
                wc2 = ifelse(wc == 1, "1", wc2)
  )

# unique(pisos_2$wc2)

data_idealista$wc2 <- factor(x = data_idealista$wc2, levels = c("1","2","3 or more"))

# str(pisos_2$wc2)


ggplot(data_idealista, aes(log(price))) +
  geom_histogram(bins = 50)

ggplot(data_idealista, aes(log(square_mt))) +
  geom_histogram(bins = 50)

data_idealista = data_idealista%>%
  dplyr::mutate(rooms2 = ifelse(rooms >= 4, "4 or more", rooms),
                rooms2 = ifelse(rooms == 3, "3", rooms2),
                rooms2 = ifelse(rooms == 2, "2", rooms2),
                rooms2 = ifelse(rooms  <= 1, "1", rooms2)
  )


data_idealista$rooms2 <- factor(x = data_idealista$rooms2, levels = c("1","2","3","4 or more"))
data_idealista$log_price <- log(data_idealista$price)

## merging terraza and blacon

data_idealista = data_idealista %>%
  mutate(terraza_balcon = ifelse((data_idealista$terraza == 1)|(data_idealista$balcon == 1),1,0))


# check

ggplot(data_idealista,aes(square_mt,price, col = distrito2)) + 
  geom_jitter()

# saving idealista data 
#### Function save the clean file with the today date
# 
# clean_df <- function(df){
#   
#   
#   
#   
#   
# }


write.csv(data_idealista,"C:/Users/ggari/Desktop/Master_MESIO/TFM/srapper/data_scrapping/idealista_data_clean_2.csv",
          fileEncoding = "UTF-8")

