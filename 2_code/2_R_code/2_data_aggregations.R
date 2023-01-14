library(tidyverse)
library(here)
library(car)

getwd()

wd = "C:/Users/ggari/Desktop/1_projects/TFM"

setwd(wd)

path_functions = here(wd,"2_code","2_R_code","functions","data_mapping.R")

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
path_data_Ide = "1_data/2_data_Idealista"

path_idealista = here(wd,path_data_Ide,"2_clean","idealista_data_clean_2.csv")

# path_idealista = here(wd,"srapper","data_scrapping","idealista_data_clean_2.csv") # already cleaned

# path_shp_barcelona = here(wd,"Barrios_de_Barcelona","0301040100_Barris_ADM_ETRS89.shp")
path_shp_barcelona = here(wd,"1_data","3_data_Barris_Barcelona","0301040100_Barris_ADM_ETRS89.shp")

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

path_open_data <- "1_data/1_data_API/dataset_opendata"


files <- list.files(here(wd,path_open_data))
# getwd()

read_API_data()


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




# terrasas ----------------------------------------------------------------

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

# Fill all NA with 0 (know that are from the barris/ districtes aggreggation)
data_idealista <- data_idealista %>% replace(is.na(.), 0)

summary(data_idealista)

saveRDS(data_idealista,file="C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/data_modelling.RDS")



