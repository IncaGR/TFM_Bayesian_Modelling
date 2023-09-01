library(tidyverse)
library(car)
library(here)
library(broom)
library(broom.mixed)

library(fastDummies)



# Constants ---------------------------------------------------------------
MAKE_PREDICTIONS = FALSE

# MODELLING DATE
# data_date = "2023-05-03"
data_date = "2023-06-05" # test sample

# PREDICT DATE
data_predict = "2023-06-05"

# upload data ready for modelling ------------------------------------------

path_modelling = paste0("data_modelling_",data_date,".RDS")

data_idealista <- readRDS(here::here('1_data','2_data_Idealista',path_modelling))

data_idealista$rooms2 <- as.factor(data_idealista$rooms2)

data_idealista$log_smt <- log(data_idealista$square_mt)

# data_idealista$rooms_ord <- factor(data_idealista$rooms2,ordered = T)

unique(data_idealista$distrito2)

data_idealista = data_idealista %>% mutate(barri_playa = ifelse(barri %in% c("la Barceloneta",
                                                                                "la Vila Olímpica del Poblenou",
                                                                                "el Poblenou",
                                                                                "Diagonal Mar i el Front Marítim del Poblenou",
                                                                                "el Besòs i el Maresme"),1,0))

# data_idealista %>% mutate(barri_playa = ifelse(barri %in% c("Distrito Ciutat Vella",
#                                                                                 "Distrito Ciutat Vella"),1,0))



regressors<-c(
  # "barri",
  "distrito2",
  "terraza",
  # "balcon",
  # "estado" ,
  # "armarios",
  # "cocina",
  "amueblado",                  
  # "planta",
  # "calef",
  "asc",
  # "aire",
  "exterior",                                    
  # "casa",
  "estudio",                    
  "wc2",
  "rooms2",
  # "rooms_ord",
  # # "terraza_balcon",
  # "n_hospitals_barri",
  # # "n_hospitals_districte",
  # "hospitals",
  # "caps",
  "cuaps",
  "n_terrazas_barri",
  # # "n_terrazas_districte",
  # # "mean_cadires_b",
  "mean_superficie_b",
  "mean_taules_b",
  # "n_arbres_bcn_barri",
  # # "n_arbres_bcn_districte",
  "n_arbres_viaris_barri",
  # # "n_arbres_viaris_districte",
  "n_bar_copas_barri",
  # # "n_bar_copas_districte",
  "square_mt",
  "new_planta",
  "flag_planta",
  "mean_income",
  "barri_playa"
)



# data_idealista[is.na(data_idealista$n_c_comercials),]$n_c_comercials <- 0

# data_idealista = data_idealista %>%
#   replace_na(list(n_c_comercials = 0,
#                   Museus = 0))

lm0 <- lm(reformulate("square_mt","log_price"),
          data_idealista)

lm0 <- lm(reformulate("square_mt + mean_income + barri_playa","log_price"),
          data_idealista)

lm1 <- lm(reformulate("square_mt + rooms","log_price"),
          data_idealista)

lm1 <- lm(reformulate("square_mt + rooms2","log_price"),
          data_idealista)

# lm1 <- lm(reformulate("square_mt + rooms_ord","log_price",intercept = F),
#           data_idealista)

lm2 <- lm(reformulate(regressors,"log_price"),
          data_idealista)

lm3 <- lm(reformulate("square_mt + asc","log_price"),
          data_idealista)

lm4 <- lm(reformulate("square_mt + asc + rooms2","log_price"),
          data_idealista)

df_x = data_idealista
# df_x = data_idealista %>% filter(flag_planta == 0)

# df_x$new_planta = as.factor(df_x$new_planta)
df_x$new_planta = as.numeric(df_x$new_planta)

lm5 <- lm(reformulate("square_mt + asc + rooms2 + new_planta + flag_planta ","log_price"),
          df_x)


# lm6 <- lm(reformulate("square_mt + asc + rooms2 + new_planta + flag_planta + asc*new_planta","log_price"),
#           df_x)

# df_x = df_x %>%
#   dplyr::mutate(wcx = ifelse(wc >= 4, "4 o mas", wc),
#                 wcx = ifelse(wc == 3, "3", wcx),
#                 wcx = ifelse(wc == 2, "2", wcx),
#                 wcx = ifelse(wc == 1, "1", wcx)
#   )

lm6 <- lm(reformulate("square_mt + asc + rooms2 + new_planta + flag_planta + wc2 + barri_playa","log_price"),
          df_x)
round((exp(coef(lm6))-1)*100,2)

plot(lm6,which = 3)



# Ad hoc analysis ---------------------------------------------------------
# Each dataset (montly data) should be different 

# df_x %>% filter(wc >5)
# 
# df_x = df_x %>% filter(!(rooms > 4 & square_mt <100)) # some strange
# df_x %>% filter((rooms > 4 & square_mt <100))

# n_barri = df_x %>% group_by(barri) %>% count() %>% arrange(n) %>% ungroup()
# 
# df_x = df_x %>% left_join(n_barri,by=join_by(barri)) %>% filter(n >10)

df_x$n_arbres_viaris_barri = as.factor(df_x$n_arbres_viaris_barri) 

df_x = df_x %>% mutate(n_arbres_viaris_barri = ifelse(barri == "el Coll",0,n_arbres_viaris_barri))

# lm7 <- lm(reformulate("distrito2 + square_mt + asc + rooms + new_planta + flag_planta + wcx +
#                       estudio + terraza + exterior + amueblado + barri_playa + mean_income + exterior*new_planta
#                       + 1","log_price"),
#           df_x,singular.ok = TRUE) # no me cuadra rooms -

names(df_x)

lm7 <- lm(log_price ~ 1 + barri + log_smt + asc + 
            # rooms2_0 +
            rooms2_1 +
            rooms2_2 +
            rooms2_3 +
            rooms2_4 +
            # + new_planta + 
            # flag_planta +
            # wc2_1 +
            wc2_2 +
            wc2_3 +
            wc2_4 +
            + terraza +
            # exterior +
            amueblado
          + lujo
          , data = df_x)
# exterior negativo? ruido?
round((exp(coef(lm7))-1)*100,2)



summary(lm0)
summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)
summary(lm5)
summary(lm6)
summary(lm7)


# x = lm(log_price ~ barri_playa, data = df_x)
# 
# summary(x)
# vif(x)


# ggplot(data_idealista,aes(rooms2,log_price)) + geom_violin()
# 
# ggplot(data_idealista,aes(año)) + geom_bar()


# con variables de open data sale max R2 .675
# sin variables open data pero con la variable barrios en vez de distrito sale 0.69

vif(lm7)

plot(lm7,ask=F)

# data_idealista[which(hatvalues(lm2)>0.99),]

# sort(cooks.distance(lm1))
cooksd =  cooks.distance(lm7)
df_x$cookd = cooks.distance(lm7)

names(df_x)

df_x = df_x %>% select(-c(X,area,id,name,zone,ubicacion_full,calle,barrio,barrio2,distrito,price_before,estado,año,datalles2,
                          cp,actualizacion,actualizacion2,extract_day,regex_barris,key_open,key_shp))

dim(df_x[df_x$cookd>0.01,])
dim(df_x[df_x$cookd>0.005,])
dim(df_x[df_x$cookd>0.002,])

plot(cooks.distance(lm7))
abline(h = 4*mean(cooksd, na.rm=T), col="red") 


# check .005
# view(df_x[df_x$cookd>0.005,])

data_cook = df_x[df_x$cookd < 0.005,]

# check .002
# view(data_cook[data_cook$cookd > 0.002,])

data_cook = data_cook[data_cook$cookd < 0.002,]

# lm3 <- lm(reformulate(regressors,"log_price"),
#           data_cook)

lm_cook <- lm(log_price ~ 1 + barri + log_smt + asc + 
                # rooms2_0 +
                rooms2_1 +
                rooms2_2 +
                rooms2_3 +
                rooms2_4 +
                # + new_planta + 
                # flag_planta +
                # wc2_1 +
                wc2_2 +
                wc2_3 +
                wc2_4 +
                + terraza +
                # exterior +
                amueblado
              + lujo
              , data = data_cook)

summary(lm_cook)

as_tibble(vif(lm_cook))

as.data.frame(vif(lm_cook))



plot(lm_cook,ask=FALSE)

modelo_cook_tidy = tidy(lm_cook)

# names(modelo_cook_tidy)

modelo_cook_tidy %>% filter(!grepl("^barri|(Intercept)",modelo_cook_tidy$term))


#TODO: mover to visualizations
ggplot(modelo_cook_tidy %>% filter(!grepl("^barri|(Intercept)",modelo_cook_tidy$term)), aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, 
                    ymax = estimate + std.error), 
                width = 0.2) +
  coord_flip() +
  ggtitle("Coefficients and Standard Deviations") +
  ylab("Coefficient") +
  xlab("Variable")


# Save data cook ----------------------------------------------------------

date_to_save <- str_extract(path_modelling, "\\d{4}-\\d{2}-\\d{2}")

path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/data_lm_cook_",date_to_save,".RDS")

saveRDS(data_cook,file=path_to_save)

save_model = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/model_cook_",date_to_save,".RDS")

saveRDS(lm_cook,file=save_model)

print(path_to_save)
print(save_model)

# save coefficients in tidy format

save_tidy = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/model_cook_tidy.RDS")

saveRDS(modelo_cook_tidy,file=save_tidy)


# predict new dataset -----------------------------------------------------

if (!(data_predict == data_date & MAKE_PREDICTIONS)){
  stop("Modelling and predicting same dataset or MAKE_PREDICTIONS equal FALSE")
}

# path_predict = paste0("data_modelling_",data_predict,".RDS")
path_predict = paste0("data_lm_cook_",data_predict,".RDS")

predict_sample <- readRDS(here::here('Desktop','1_projects','TFM','1_data','2_data_Idealista',path_predict))

table(predict_sample$wc2)
table(predict_sample$wc)

predict_sample$rooms2 <- as.factor(predict_sample$rooms2)

# predict_sample = predict_sample %>% filter(lujo == 0)

predict_sample = predict_sample %>% filter(barri != "Ciutat Meridiana") # removing Ciutat Meridiana

predictions = exp(predict.lm(lm_cook,predict_sample))


# Compute the actual mean price for each observation in the test data
real_values <- exp(predict_sample$log_price)

# Compute a measure of predictive performance
RMSE <- sqrt(mean((predictions - real_values)^2))
print(RMSE)

plot(real_values,predictions)


predict_sample$y_tilde = predictions


names(predict_sample)

rsquared = 1 - (sum((real_values - predictions)^2)/sum((real_values - mean(real_values))^2))

print(rsquared)


