library(rstanarm)
options(mc.cores = parallel::detectCores())


# let's try the rstanrm

data_date = "2023-05-03"

path_modelling = paste0("data_lm_cook_",data_date,".RDS")

data_cook<- readRDS(here::here('Desktop','1_projects','TFM','1_data','2_data_Idealista',path_modelling))

data_cook$barri <- as.factor(data_cook$barri)

names(data_cook)

stan_lmer(log_price ~ log(square_mt) + (1))