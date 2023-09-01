library(tidyverse)
library(car)
library(here)
library(broom)
library(broom.mixed)
library(rstan)
library(bayesplot)


# upload data ready for modelling ------------------------------------------
options(mc.cores = parallel::detectCores())
rstan_options(auto_write=TRUE )


# install.packages("https://cran.r-project.org/src/contrib/Archive/StanHeaders/StanHeaders_2.21.0-7.tar.gz",
#                  type="source",repos=NULL)
packageVersion("StanHeaders")
packageVersion("rstan")



data_date = "2023-05-03"

path_modelling = paste0("data_lm_cook_",data_date,".RDS")

data_cook<- readRDS(here::here('Desktop','1_projects','TFM','1_data','2_data_Idealista',path_modelling))

data_cook$barri <- as.factor(data_cook$barri)

# no lujo?
data_cook = data_cook %>% filter(lujo == 0)

names(data_cook)
# price hierarchical covariate --------------------------------------------

# VARIABLES USED IN OLS
# lm7 <- lm(log_price ~ 1 + barri + square_mt + asc + rooms2 + new_planta + 
#             flag_planta + wc2 + terraza + exterior + amueblado
#           + lujo
#           , data = df_x)

N= nrow(data_cook)
barri_name <- unique(data_cook$barri)
barri <- as.numeric(data_cook$barri)
J <- length(unique(barri))
y <- data_cook$log_price
x1 <- log(data_cook$square_mt)
x2 <- data_cook$rooms2_1
x3 <- data_cook$rooms2_2
x4 <- data_cook$rooms2_3
x5 <- data_cook$rooms2_4
x6 <- data_cook$asc
x7 <- data_cook$wc2_2
x8 <- data_cook$wc2_3
x9 <- data_cook$wc2_4
x10 <- data_cook$terraza
x11 <- data_cook$amueblado
# x12 <- data_cook$lujo

mean_income <- data_cook %>%
  group_by(barri) %>%
  summarise(mean_income = first(log(mean_income))) %>%
  pull(mean_income)


data_list <- list(
  N = N,
  y = y,
  J = J,
  x1 = x1,
  x2 = x2,
  x3 = x3,
  x4 = x4,
  x5 = x5,
  x6 = x6,
  x7 = x7,
  x8 = x8,
  x9 = x9,
  x10 = x10,
  x11 = x11,
  # x12 = x12,
  barri = barri,
  mean_income = mean_income
)

# mean_atur <- data_cook %>%
#   group_by(barri) %>%
#   summarise(mean_atur = first(mean_perc_atur)) %>%
#   pull(mean_atur) # no aporta

# playa <- data_cook %>%
#   group_by(barri) %>%
#   summarise(playa = first(barri_playa)) %>%
#   pull(playa)

# data_list <- list(
#   N = N,
#   J = J,
#   y = y,
#   x1 = x1, # log square mt
#   x2 = x2, # nº rooms
#   x3 = x3, # nº wc
#   # x4 = x4, # lujo
#   x5 = x5,
#   terraza = terraza,
#   # playa = playa,
#   barri = barri,
#   
#   # ,mean_atur = mean_atur
#   
# )


# Varying intercept model -------------------------------------------------

# model_code <- "
# data {
#   int<lower=0> N;
#   int<lower=0> J;
#   vector[N] y; // log price
#   real x1[N]; // log square mt
#   int x2[N]; // nº rooms
#   int x3[N]; // nº wc
#   // int x4[N]; // binari lujo
#   int x5[N]; // binari elevator
#   int terraza[N];
#   int barri[N];
#   // int playa[J]; no effect
#   vector[J] mean_income;
#   //vector[J] mean_atur; no effect
# }
# parameters {
#   real a[J]; // intercept each barri
#   real<lower=0> b; // log square mt
#   real c; // nº rooms
#   real<lower=0> d; // numeric nº wc
#   //real<lower=0> e; // lujo
#   real<lower=0> f; // asc
#   real<lower=0> b6; // terraza
#   real g_0;
#   real g_1;
#   // real g_2; no effect
#   //real g_3; no effect
#   real<lower=0> sigma_y;
#   real<lower=0> sigma_a;
# }
# model {
# 
#   sigma_y ~ cauchy(0, 10);
#   sigma_a ~ cauchy(0, 10);
#   b ~ cauchy(0,2.5);
#   c ~ cauchy(0,2.5);
#   d ~ cauchy(0,2.5);
#   //e ~ cauchy(0,2.5);
#   f ~ cauchy(0,2.5);
#   b6 ~ cauchy(0,2.5);
#   for (j in 1:J)
#     a[j] ~ normal(g_0 + g_1 * mean_income[j], sigma_a);
#   for (n in 1:N)
#     y[n] ~ normal(a[barri[n]] + b * x1[n] + c * x2[n] + d * x3[n] 
#     //+ e * x4[n]
#      + f * x5[n] + b6 * terraza[n] , sigma_y);
# }
# "
model_code <- "
data {
  int<lower=0> N;
  int<lower=0> J;
  vector[N] y;
  real x1[N];
  int x2[N];
  int x3[N];
  int x4[N];
  int x5[N];
  int x6[N];
  int x7[N];
  int x8[N];
  int x9[N];
  int x10[N];
  int x11[N];
  // int x12[N];
  int barri[N];
  vector[J] mean_income;
}
parameters {
  real b0[J];
  
  real log_smt; // log square mt
  real rooms2_1 ; // rooms
  real rooms2_2; // rooms
  real rooms2_3; // rooms
  real rooms2_4; // rooms
  real asc; // asc
  real wc2_2; // wc
  real wc2_3; // wc
  real wc2_4; // wc
  real terraza;
  real amueblado;
  // real lujo; 
  
  real g_0;
  real g_1;
  
  real<lower=0> sigma_y;
  real<lower=0> sigma_a;
}
model {
  sigma_y ~ cauchy(0, 10);
  sigma_a ~ cauchy(0, 10);
 // g_0 ~ cauchy(0,10);
 // g_1 ~cauchy(0,10);
  log_smt ~ cauchy(0,2.5);
  rooms2_1 ~ cauchy(0,2.5);
  rooms2_2 ~ cauchy(0,2.5);
  rooms2_3 ~ cauchy(0,2.5);
  rooms2_4 ~ cauchy(0,2.5);
  asc ~ cauchy(0,2.5);
  wc2_2 ~ cauchy(0,2.5);
  wc2_3 ~ cauchy(0,2.5);
  wc2_4 ~ cauchy(0,2.5);
  terraza ~ cauchy(0,2.5);
  amueblado ~ cauchy(0,2.5);
  // lujo ~ cauchy(0,2.5);

  for (j in 1:J)
    b0[j] ~ normal(g_0 + g_1 * mean_income[j], sigma_a);
  
  for (n in 1:N)
    y[n] ~ normal(b0[barri[n]] + log_smt * x1[n] + rooms2_1 * x2[n] + 
              rooms2_2 * x3[n] + rooms2_3 * x4[n] + rooms2_4 * x5[n] + 
              asc * x6[n] + wc2_2 * x7[n] + wc2_3 * x8[n] + wc2_4 * x9[n] + 
              terraza * x10[n] + amueblado * x11[n]
              // + lujo * x12[n]
              , sigma_y);
}
"



# Varying slope change ----------------------------------------------------

# model_code <- "
# data {
#   int<lower=0> N;
#   int<lower=0> J;
#   vector[N] y; // log price
#   real x1[N]; // log square mt
#   int x2[N]; // nº rooms
#   int x3[N]; // nº wc
#   int x4[N]; // binari lujo 
#   int x5[N]; // binari elevator
#   int terraza[N];
#   int barri[N];
#   vector[J] mean_income;
# }
# parameters {
#   real a[J]; // intercept each barri
#   real b[J]; // slope for each barri
#   real c; // nº rooms
#   real<lower=0> d; // numeric nº wc
#   real<lower=0> e; // lujo
#   real<lower=0> f; // asc
#   real<lower=0> b6; // terraza
#   real g_0;
#   real g_1;
#   real h_0; // intercept for slope
#   real h_1; // slope for slope
#   real<lower=0> sigma_y;
#   real<lower=0> sigma_a;
#   real<lower=0> sigma_b; // standard deviation for slope
# }
# model {
# 
#   sigma_y ~ cauchy(0, 10);
#   sigma_a ~ cauchy(0, 10);
#   sigma_b ~ cauchy(0, 10);
#   c ~ cauchy(0,2.5);
#   d ~ cauchy(0,2.5);
#   e ~ cauchy(0,2.5);
#   f ~ cauchy(0,2.5);
#   b6 ~ cauchy(0,2.5);
#   for (j in 1:J) {
#     a[j] ~ normal(g_0 + g_1 * mean_income[j], sigma_a);
#     b[j] ~ normal(h_0 + h_1 * mean_income[j], sigma_b); // slope varying by barri and mean_income
#   }
#   for (n in 1:N)
#     y[n] ~ normal(a[barri[n]] + b[barri[n]] * x1[n] + c * x2[n] + d * x3[n] + e * x4[n]
#      + f * x5[n] + b6 * terraza[n] , sigma_y);
# }
# "


translate = stanc(model_code  = model_code)

model = stan_model(stanc_ret = translate)

# Fit the model to the data
fit_4 <- sampling(model, data = data_list, chains = 4, iter =5000, verbose = TRUE, seed = 132) # 4000?

# ## Convergence analysis
print(fit_4) # Cuando hay porblemas de multicolinearidad max depth sube y r-hat
# plot(fit_4)

# Save hierarchical model -------------------------------------------------

# path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4.RDS")
# path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_no_log.RDS") # test no log square mt
# path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_priors.RDS") # with priors
# path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_1.RDS") # wc cov
# path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_2.RDS") # terrace
# path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_2_1.RDS") # lujo cauchy narrow
# path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_3.RDS") # asc
# path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_4.RDS") # terraza
# path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_5.RDS") # playa
# path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_6.RDS") # intercept: playa+renta
# path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_7.RDS") # varying the slope # no converge
# path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_8.RDS") # no lujo data
path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_9.RDS") # all variables


saveRDS(fit_4, path_to_save)


# read fit ----------------------------------------------------------------

fit_4 <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_9.RDS")

# fit_4
# predicting new unit -----------------------------------------------------

typeof(fit_4)

levels(barri_name)

sims <- rstan::extract(fit_4)

# new unit in an existing group 
a <- sims$a
b <- sims$b
c <- sims$c
d <- sims$d
e <- sims$e
f <- sims$f
b6 <- sims$b6
# b7 <- sims$b7
sigma.y <- sims$sigma_y
n.sims <- nrow(a)
y.tilde <- rnorm(n.sims, a[,40] + b * log(90) + c * 3 + d * 2 + e * 0 + f * 1 + b6 * 1 , sigma.y)

# exp(y.tilde)
hist(y.tilde)
hist(exp(y.tilde))
mean(exp(y.tilde))

# model diagnostic --------------------------------------------------------
plot(fit_4)
plot(fit_4,plotfun = "rhat")
plot(fit_4, plotfun = "trace", pars = c("b", "c","d","e","f","b6"), inc_warmup = TRUE)
plot(fit_4, show_density = TRUE, pars=c("b","c","d","e","f","b6"), ci_level = 0.8, fill_color = "purple")



