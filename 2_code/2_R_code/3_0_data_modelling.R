library(tidyverse)
library(car)
library(here)
library(broom)
library(broom.mixed)


# upload data ready for modelling ------------------------------------------

# data_date = "2023-05-03"
data_date = "2023-06-05" # test sample



path_modelling = paste0("data_modelling_",data_date,".RDS")

data_idealista <- readRDS(here::here('Desktop','1_projects','TFM','1_data','2_data_Idealista',path_modelling))

data_idealista$rooms2 <- as.factor(data_idealista$rooms2)

# data_idealista$rooms_ord <- factor(data_idealista$rooms2,ordered = T) 

# bar de copas

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
  "mean_income"
)

# data_idealista[is.na(data_idealista$n_c_comercials),]$n_c_comercials <- 0

# data_idealista = data_idealista %>%
#   replace_na(list(n_c_comercials = 0,
#                   Museus = 0))

lm0 <- lm(reformulate("square_mt","log_price"),
          data_idealista)

lm0 <- lm(reformulate("square_mt + mean_income","log_price"),
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

df_x = df_x %>%
  dplyr::mutate(wcx = ifelse(wc >= 4, "4 o mas", wc),
                wcx = ifelse(wc == 3, "3", wcx),
                wcx = ifelse(wc == 2, "2", wcx),
                wcx = ifelse(wc == 1, "1", wcx)
  )

lm6 <- lm(reformulate("square_mt + asc + rooms2 + new_planta + flag_planta + wcx","log_price"),
          df_x)
round((exp(coef(lm6))-1)*100,2)

plot(lm6,which = 3)



# Ad hoc analysis ---------------------------------------------------------
# Each dataset (montly data) should be different 

df_x %>% filter(wc >5)

df_x = df_x %>% filter(!(rooms > 4 & square_mt <100)) # some strange
df_x %>% filter((rooms > 4 & square_mt <100))

n_barri = df_x %>% group_by(barri) %>% count() %>% arrange(n) %>% ungroup()

df_x = df_x %>% left_join(n_barri,by=join_by(barri)) %>% filter(n >10)

df_x$n_arbres_viaris_barri = as.factor(df_x$n_arbres_viaris_barri) 

df_x = df_x %>% mutate(n_arbres_viaris_barri = ifelse(barri == "el Coll",0,n_arbres_viaris_barri))

lm7 <- lm(reformulate("barri + square_mt + asc + rooms + new_planta + flag_planta + wcx + estudio + terraza + exterior + amueblado + 1","log_price"),
          df_x,singular.ok = TRUE) # no me cuadra rooms -
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
# 
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
                   cp,actualizacion,actualizacion2,extract_day,regex_barris,key_open,key_shp,
                   n))

dim(df_x[df_x$cookd>0.01,])
dim(df_x[df_x$cookd>0.005,])
dim(df_x[df_x$cookd>0.002,])

plot(cooks.distance(lm7))
abline(h = 4*mean(cooksd, na.rm=T), col="red") 

# outlierTest(lm1)
# cook_test

# cook_test = data_idealista[data_idealista$cookd >(4*mean(cooksd, na.rm=T)),]

# data_cook = data_idealista[data_idealista$cookd < 0.005,]
# data_cook = data_idealista[data_idealista$cookd < 0.01,]

# test <- c(1479,2777,2825)

# data_cook = data_idealista[-test,]
df_x[df_x$cookd>0.005,]

data_cook = df_x[df_x$cookd < 0.005,]
data_cook[data_cook$cookd > 0.002,]

data_cook = data_cook[data_cook$cookd < 0.002,]

# lm3 <- lm(reformulate(regressors,"log_price"),
#           data_cook)

lm_cook <- lm(reformulate("barri + square_mt + asc + rooms + new_planta + flag_planta + wcx + estudio + terraza + exterior + amueblado + 1","log_price"),
              data_cook)

summary(lm_cook)

# plot(lm_cook)




date_to_save <- str_extract(path_modelling, "\\d{4}-\\d{2}-\\d{2}")

path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/data_lm_cook_",date_to_save,".RDS")

saveRDS(data_cook,file=path_to_save)


# # Extraño que con mas habitaciones menos precio, contra intuitivo.
# betas <- as.data.frame(dfbetas(lm3))
# 
# summary(betas$rooms)
# 
# betas %>%filter(rooms == min(rooms))
# 
# test_row_name = row.names(betas %>%filter(rooms == min(rooms)))
# 
# tmp =data_cook %>% filter(rownames(data_cook) != test_row_name)
# 
# lm3 <- lm(reformulate("square_mt + asc + amueblado + aire + 
#                       estudio + rooms + wc2 + n_arbres_viaris_barri + cuaps
#                       ","log_price"),
#           tmp)
# 
# summary(lm3)
# 
# 
# 
# colnames(betas) <- gsub(pattern = "[()]|[ ]","",colnames(betas))
# 
# colnames(betas) <-  paste0(colnames(betas),'_beta')
# 
# data_betas <- cbind(data_cook,betas)
# 
# data_betas %>%filter(rooms_beta == min(rooms_beta)) 
# 
# 
# data_betas = data_betas %>%filter(!rooms_beta == min(rooms_beta)) 
# 
# 
# 
# 
# lm3 <- lm(reformulate("square_mt + asc + amueblado + aire + 
#                       estudio + rooms + wc2 + n_arbres_viaris_barri + cuaps
#                       ","log_price"),
#           data_betas)
# 
# data_betas <- data_betas %>% select(- contains("_beta"))
# 
# 
# betas <- as.data.frame(dfbetas(lm3))
# 
# 
# betas %>%filter(rooms == min(rooms))
# 
# colnames(betas) <- gsub(pattern = "[()]|[ ]","",colnames(betas))
# 
# colnames(betas) <-  paste0(colnames(betas),'_beta')
# 
# data_betas <- cbind(data_betas,betas)
# 
# data_betas %>%filter(rooms_beta == min(rooms_beta)) 
# 
# 
# data_betas = data_betas %>%filter(!rooms_beta == min(rooms_beta)) 
# 
# 
# lm3 <- lm(reformulate("square_mt + asc + amueblado + aire + 
#                       estudio + rooms + wc2 + n_arbres_viaris_barri + cuaps
#                       ","log_price"),
#           data_betas)
# data_betas <- data_betas %>% select(- contains("_beta"))
# 
# 
# betas <- as.data.frame(dfbetas(lm3))
# 
# 
# betas %>%filter(rooms == min(rooms))
# 
# 
# colnames(betas) <- gsub(pattern = "[()]|[ ]","",colnames(betas))
# 
# colnames(betas) <-  paste0(colnames(betas),'_beta')
# 
# data_betas <- cbind(data_betas,betas)
# 
# data_betas %>%filter(rooms_beta == min(rooms_beta)) 
# 
# data_betas = data_betas %>%filter(!rooms_beta == min(rooms_beta)) 
# 
# 
# lm3 <- lm(reformulate("square_mt + asc + amueblado + aire + 
#                       estudio + rooms + wc2 + n_arbres_viaris_barri + cuaps
#                       ","log_price"),
#           data_betas)
# 
# # con variables de open data sale max R2 .675
# # sin variables open data pero con la variable barrios en vez de distrito sale 0.69
# 
# 
# summary(lm3)
# vif(lm3)
# 
# plot(lm3,ask=F)


# some outliers, now the plots are better
# data_idealista[2777,]
# data_idealista[2825,]
# data_idealista[1479,]
# Modeling Bayesian ------------------------------------------------------

# remove.packages(c("StanHeaders", "rstan"))
# install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

# library("rstan")
# library(bayesplot)
# # First we are going to try a simple bayesian model with covariate square_mt.
# 
# options(mc.cores = parallel::detectCores())
# 
# rstan_options(auto_write = TRUE)
# 
# # price_pooled ------------------------------------------------------------
# 
# N= nrow(data_cook)
# barri <- as.numeric(data_cook$id_barri)
# barri_name <- as.factor(unique(data_cook$barri))
# J <- length(unique(barri))
# y <- data_cook$log_price
# x <- log(data_cook$square_mt)
# # u <- radon %>% 
# #   group_by(county) %>% 
# #   summarise(u = first(log_uranium)) %>% 
# #   pull(u) # because is value per county 
# 
# 
# data_list <- list(
#   N = N,
#   y = y,
#   x = x
# )
# 
# model_code <- "
# data {
#   int<lower=0> N;
#   vector[N] y;
#   vector<lower=0>[N] x;
# }
# parameters {
#   real a;
#   real b;                           
#   real<lower=0> sigma_y;
# }
# model {
#   y ~ normal(a + b * x, sigma_y);
# }
# "
# 
# model = stan_model(model_code = model_code)
# 
# # Fit the model to the data
# fit <- sampling(model, data = data_list, chains = 2, iter =2000)
# 
# 
# print(fit)
# plot(fit)
# 
# 
# 
# price_summary <- tidy(fit, conf.int = T, level = 0.8, rhat = T, ess = T)
# 
# df_pooled  <- tibble(
#   barri = barri_name,
#   model = "pooled",
#   intercept = price_summary$estimate[1],
#   slope = price_summary$estimate[2]
# )
# 
# # 
# # id_barrio<- c("la Guineueta",
# #                "la Vall d'Hebron", "Canyelles", "la Trinitat Nova", "el Raval", "la Dreta de l'Eixample", "el Barri Gòtic",
# #                "Sants")
# 
# 
# df_model <- df_pooled %>%
#   left_join(data_cook, by = "barri") 
# # %>%
# #   filter(barri %in% id_barrio)
# 
# # ggplot(df_model) +
# #   geom_point(aes(log(square_mt), log_price)) +
# #   geom_abline(aes(intercept = intercept, slope = slope, color = model)) +
# #   facet_wrap(~ barri, ncol = 4) +
# #   theme(legend.position = "bottom")
# 
# # Save document
# 
# date_to_save <- str_extract(path_modelling, "\\d{4}-\\d{2}-\\d{2}")
# 
# path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/data_modeled_pooled_",date_to_save)
# 
# saveRDS(df_model,file=path_to_save)
# 
# 
# # price no pooled ---------------------------------------------------------
# 
# data_list <- list(
#   N = N,
#   J = J,
#   y = y,
#   x = x,
#   barri = barri
# )
# 
# # radon_2 <- stan("stan_models/radon_2_no_pooled_a.stan", iter = 1000, chains = 1,
# #                 data = data_list, seed = 1)
# 
# model_code <- "
# data {
#   int<lower=0> N;
#   int<lower=0> J;
#   vector[N] y;
#   vector<lower=0>[N] x;
#   int barri[N];
# }
# parameters {
#   real a[J];
#   real b;              
#   real<lower=0> sigma_y;
# }
# model {
#   for (i in 1:N)
#     y[i] ~ normal(a[barri[i]] + b * x[i], sigma_y);
# }
# "
# 
# # data {
# #   int<lower=0> N;
# #   vector[N] y;
# #   vector<lower=0>[N] x;
# # }
# # parameters {
# #   real a;
# #   real b;                           
# #   real<lower=0> sigma_y;
# # }
# # model {
# #   y ~ normal(a + b * x, sigma_y);
# # }
# 
# model_2 = stan_model(model_code = model_code)
# 
# # Fit the model to the data
# fit_2 <- sampling(model_2, data = data_list, chains = 1, iter =100)
# 
# 
# ## Convergence analysis
# print(fit_2)
# plot(fit_2)
# 
# price_summary <- tidy(fit_2, conf.int = T, level = 0.8, rhat = T, ess = T)
# 
# df_no_pooled  <- tibble(
#   county = barri_name,
#   model = "no_pooled",
#   intercept = price_summary$estimate[1:J],
#   slope = price_summary$estimate[J+1]
# )
# 
# df_model <- bind_rows(df_pooled, df_no_pooled) %>% 
#   left_join(data_cook, by = "barri") %>%
#   filter(barri %in% id_barrio) 
# 
# ggplot(df_model) +
#   geom_jitter(aes(log(square_mt), log_price)) +
#   geom_abline(aes(intercept = intercept, slope = slope, color = model)) +
#   facet_wrap(~ barri, ncol = 4) + 
#   scale_x_continuous(breaks = 0:1) + 
#   theme(legend.position = "bottom")
# 
