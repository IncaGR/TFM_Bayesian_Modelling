library(tidyverse)
library(here)
library(car)
# library(dplyr)
library(lubridate)
# library(stringr)

getwd()


# date_of_data = "2023-05-03" # put the date of the file you want to clean
date_of_data = "2023-06-05" # test sample


path_idealista_folder = paste("extraction",date_of_data, sep = "_")
path_idealista_csv = paste0("datos_scrapping_",date_of_data,".csv")

wd = "C:/Users/ggari/Desktop/1_projects/TFM"

path_functions = here(wd,"2_code","2_R_code","functions","data_mapping.R")

source(path_functions)


# Reading data ------------------------------------------------------------
path_data_Ide = "1_data/2_data_Idealista"

# path_idealista = here(wd, path_data_Ide,"1_raw","extraction_2022-11-01","datos_scrapping_2022-11-01.csv")
# path_idealista = here(wd,path_data_Ide,"1_raw","extraction_2022-11-17","datos_scrapping_2022-11-19.csv")
path_idealista = here(wd,path_data_Ide,"1_raw",path_idealista_folder,path_idealista_csv)

path_shp_barcelona = here(wd,"1_data","3_data_Barris_Barcelona","0301040100_Barris_ADM_ETRS89.shp")

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


# to connect Idealista data and shp file the key is: key_shp
data_idealista = data_idealista %>%
  mutate(key_shp = regex_barris)


data_idealista = data_idealista %>%
  dplyr::rename(id = X.U.FEFF.id)


# data cleaning -----------------------------------------------------------


ggplot(data_idealista,aes(square_mt,price)) + 
  geom_jitter()


# data_idealista = data_idealista[data_idealista$price<70000,]
# data_idealista = data_idealista[data_idealista$price<10000,] # testing 
# data_idealista = data_idealista[data_idealista$price<7500,] # testing 

data_idealista = data_idealista[data_idealista$square_mt>10,] # casa con 0 metros, eliminada
# data_idealista = data_idealista[data_idealista$square_mt<375,]

## Adding new binary variables
data_idealista = data_idealista %>%
  mutate(casa = ifelse(grepl("Casa o chalet",data_idealista$name)==TRUE,1,0))

data_idealista$casa <- as.factor(data_idealista$casa)

# minimo precio considerado en Idealista como lujo 5000
data_idealista = data_idealista %>%
  mutate(lujo = ifelse(price>=5000,1,0))

data_idealista$lujo <- as.factor(data_idealista$lujo)

data_idealista = data_idealista %>%
  mutate(estudio = ifelse(grepl("Estudio",data_idealista$name)==TRUE,1,0))

data_idealista$estudio <- as.factor(data_idealista$estudio)


ggplot(data_idealista,aes(square_mt,price,color = lujo)) + 
  geom_jitter()

ggplot(data_idealista,aes(square_mt,price,color = casa)) + 
  geom_jitter()

ggplot(data_idealista,aes(square_mt,price,color = estudio)) + 
  geom_jitter(alpha= 0.4)

## Convert to factor some variables

# unique(pisos_2$wc)

table(data_idealista$wc)


data_idealista = data_idealista %>%
  dplyr::mutate(wc2 = ifelse(wc >= 4, "4 o mas", wc),
                wc2 = ifelse(wc == 3, "3", wc2),
                wc2 = ifelse(wc == 2, "2", wc2),
                wc2 = ifelse(wc == 1, "1", wc2)
  )

table(data_idealista$wc2)

# unique(pisos_2$wc2)

data_idealista$wc2 <- factor(x = data_idealista$wc2, levels = c("1","2","3","4 o mas"))

# str(pisos_2$wc2)


ggplot(data_idealista, aes(log(price))) +
  geom_histogram(bins = 50)

ggplot(data_idealista, aes(log(square_mt))) +
  geom_histogram(bins = 50)

table(data_idealista$rooms)

data_idealista = data_idealista%>%
  dplyr::mutate(rooms2 = ifelse(rooms >= 4, "4 o mas", rooms),
                rooms2 = ifelse(rooms == 3, "3", rooms2),
                rooms2 = ifelse(rooms == 2, "2", rooms2),
                rooms2 = ifelse(rooms  == 1, "1", rooms2),
                rooms2 = ifelse(rooms == 0, "0 estudio", rooms2)
  )

table(data_idealista$rooms2)

data_idealista$rooms2 <- factor(x = data_idealista$rooms2, levels = c("0 estudio","1","2","3","4 o mas"))
data_idealista$log_price <- log(data_idealista$price)

ggplot(data_idealista,aes(x = (rooms2), y = price)) + 
  geom_boxplot() +
  geom_jitter(width=0.2)

## merging terraza and blacon

data_idealista = data_idealista %>%
  mutate(terraza_balcon = ifelse((data_idealista$terraza == 1)|(data_idealista$balcon == 1),1,0))


# check

ggplot(data_idealista,aes(square_mt,price, col = distrito2)) + 
  geom_jitter()

# adding planta

ggplot(data_idealista,aes(planta)) + geom_bar()

# data_idealista$new_planta <- as.numeric(stringr::str_extract(data_idealista$planta,"\\d+"))

data_idealista = data_idealista %>% mutate(flag_planta = ifelse(planta=="0",1,0),
                          new_planta = ifelse(planta=="bajo",0,as.numeric(stringr::str_extract(planta,"\\d+")))) # imputted when scrapping

ggplot(data_idealista,aes(new_planta)) + geom_bar()

# Extract date using regular expression
date_to_save <- str_extract(path_idealista, "\\d{4}-\\d{2}-\\d{2}")

path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/2_clean/data_idealista_clean_",date_to_save,".csv")

write.csv(data_idealista,path_to_save,
          fileEncoding = "UTF-8")
print(paste0("Succesfully saved data of: ",date_to_save))
