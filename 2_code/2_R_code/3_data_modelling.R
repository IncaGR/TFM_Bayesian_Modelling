library(tidyverse)
library(car)
library(here)


# upload data ready for modelling ------------------------------------------

data_idealista <- readRDS(here::here('Desktop','1_projects','TFM','1_data','2_data_Idealista','data_modelling.RDS'))

# bar de copas
colnames(data_idealista)

regressors<-c(
  # "barri",
  "distrito2",
  "terraza",
  # "balcon",
  "estado" ,
  # "armarios",
  # "cocina",
  "amueblado",                  
  # "planta",
  # "calef",
  "asc",
  "aire",
  "exterior",                                    
  # "casa",
  "estudio",                    
  "wc2",
  "rooms2",
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
  "square_mt"
)

# data_idealista[is.na(data_idealista$n_c_comercials),]$n_c_comercials <- 0

# data_idealista = data_idealista %>%
#   replace_na(list(n_c_comercials = 0,
#                   Museus = 0))


lm0 <- lm(reformulate("square_mt","log_price"),
          data_idealista)

lm1 <- lm(reformulate("square_mt + rooms2","log_price"),
          data_idealista)

lm2 <- lm(reformulate(regressors,"log_price"),
          data_idealista)


summary(lm0)
summary(lm1)
summary(lm2)


# con variables de open data sale max R2 .675
# sin variables open data pero con la variable barrios en vez de distrito sale 0.69

vif(lm2)

plot(lm2,ask=F)

# data_idealista[which(hatvalues(lm2)>0.99),]

# sort(cooks.distance(lm1))
cooksd =  cooks.distance(lm2)
data_idealista$cookd = cooks.distance(lm2)


dim(data_idealista[data_idealista$cookd>0.01,])

plot(cooks.distance(lm2))
abline(h = 4*mean(cooksd, na.rm=T), col="red") 

# outlierTest(lm1)
# cook_test

# cook_test = data_idealista[data_idealista$cookd >(4*mean(cooksd, na.rm=T)),]

# data_cook = data_idealista[data_idealista$cookd < 0.005,]
# data_cook = data_idealista[data_idealista$cookd < 0.01,]
test <- c(1479,2777,2825)

data_cook = data_idealista[-test,]
data_cook = data_cook[data_cook$cookd < 0.005,]



lm3 <- lm(reformulate(regressors,"log_price"),
          data_cook)

# con variables de open data sale max R2 .675
# sin variables open data pero con la variable barrios en vez de distrito sale 0.69


summary(lm3)
vif(lm3)

plot(lm3,ask=F)


# some outliers, now the plots are better
# data_idealista[2777,]
# data_idealista[2825,]
# data_idealista[1479,]
# Modeling Bayesian ------------------------------------------------------

# remove.packages(c("StanHeaders", "rstan"))
# install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

library("rstan")
library(bayesplot)
# First we are going to try a simple bayesian model with covariate square_mt.

options(mc.cores = parallel::detectCores())

rstan_options(auto_write = TRUE)

# # Define the Stan model
# # Define the Stan model
# model_code <- "
# data {
#   int<lower=0> N;
#   vector[N] x;
#   vector[N] y;
# }
# parameters {
#   real alpha;
#   real beta;
#   real<lower=0> sigma;
# }
# model {
#   y ~ normal(alpha + beta * x, sigma);
#   
# }
# generated quantities {
#   vector[N] y_sim;
#   for (i in 1:N) {
#     y_sim[i] <- normal_rng(alpha + beta * x[i], sigma);
#   }
# }
# "
# 
# 
# n = nrow(data_cook)
# 
# # Compile the model
# model = stan_model(model_code = model_code)
# 
# # Fit the model to the data
# fit <- sampling(model, data = list(N = n, x = data_cook$square_mt, y = data_cook$price))
# 
# print(fit)
# 
# # Extract the estimated parameters
# alpha <- extract(fit)$alpha[1]
# beta <- extract(fit)$beta[1]
# sigma <- extract(fit)$sigma[1]
# 
# 
# # Plot the trace plots
# par(mfrow = c(2, 2))
# plot(fit, pars = c("alpha", "beta", "sigma"))
# 
# 
# # Plot the posterior predictive checks
# y_sim <- extract(fit)$y_sim
# posterior_predictive_check <- data.frame(y_observed = data_cook$price, y_sim = y_sim[1,])
# ggplot(posterior_predictive_check, aes(y_observed, y_sim)) + 
#   geom_point() + 
#   geom_abline(intercept = 0, slope = 1) + 
#   xlab("Observed y") + 
#   ylab("Simulated y") + 
#   ggtitle("Posterior Predictive Checks")
# 
# 
# traceplot(fit)
# Some simulated prices are negative, this cannot be the case in price.




# remove.packages(c("rstan","StanHeaders"))
# if (file.exists(".RData")) file.remove(".RData")
# 
# Sys.setenv(MAKEFLAGS = paste0("-j",parallel::detectCores()))
# 
# install.packages(c("StanHeaders","rstan"),type="source")





# price_pooled ------------------------------------------------------------

N= nrow(data_cook)
barri <- as.numeric(data_cook$barri)
barri_name <- unique(data_cook$barri)
J <- length(unique(barri))
y <- data_cook$log_price
x <- log(data_cook$square_mt)
# u <- radon %>% 
#   group_by(county) %>% 
#   summarise(u = first(log_uranium)) %>% 
#   pull(u) # because is value per county 


data_list <- list(
  N = N,
  y = y,
  x = x
)

# price_1 <- stan("2_code\2_R_code\stan_models\1_price_pooled.stan", iter = 500, chains = 1,
#                 data = data_list, seed = 1)
# price_1

model_code <- "
data {
  int<lower=0> N;
  vector[N] y;
  vector<lower=0>[N] x;
}
parameters {
  real a;
  real b;                           
  real<lower=0> sigma_y;
}
model {
  y ~ normal(a + b * x, sigma_y);
}
"

model = stan_model(model_code = model_code)

# Fit the model to the data
fit <- sampling(model, data = data_list)


print(fit)
plot(fit)



price_summary <- tidy(fit, conf.int = T, level = 0.8, rhat = T, ess = T)

df_pooled  <- tibble(
  barri = barri_name,
  model = "pooled",
  intercept = price_summary$estimate[1],
  slope = price_summary$estimate[2]
)

id_barrio<- c("la Guineueta",
               "la Vall d'Hebron", "Canyelles", "la Trinitat Nova", "el Raval", "la Dreta de l'Eixample", "el Barri Gòtic",
               "Sants")


df_model <- df_pooled %>%
  left_join(data_cook, by = "barri") %>%
  filter(barri %in% id_barrio)

ggplot(df_model) +
  geom_point(aes(log(square_mt), log_price)) +
  geom_abline(aes(intercept = intercept, slope = slope, color = model)) +
  facet_wrap(~ barri, ncol = 4) +
  theme(legend.position = "bottom")
