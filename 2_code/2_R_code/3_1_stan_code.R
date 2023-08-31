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

data_cook<- readRDS(here::here('1_data','2_data_Idealista',path_modelling))

data_cook$barri <- as.factor(data_cook$barri)


# price_pooled ------------------------------------------------------------



# lm_cook <- lm(log_price ~ 1 + barri + log_smt + asc + 
#                 # rooms2_0 +
#                 rooms2_1 +
#                 rooms2_2 +
#                 rooms2_3 +
#                 rooms2_4 +
#                 # + new_planta + 
#                 # flag_planta +
#                 # wc2_1 +
#                 wc2_2 +
#                 wc2_3 +
#                 wc2_4 +
#                 + terraza +
#                 # exterior +
#                 amueblado
#               + lujo
#               , data = data_cook)
# 
# summary(lm_cook)


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
x12 <- data_cook$lujo



data_list <- list(
  N = N,
  y = y,
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
  x12 = x12
)

model_code <- "
data {
  int<lower=0> N;
  vector[N] y;
  vector<lower=0>[N] x1;
  vector<lower=0>[N] x2;
  vector<lower=0>[N] x3;
  vector<lower=0>[N] x4;
  vector<lower=0>[N] x5;
  vector<lower=0>[N] x6;
  vector<lower=0>[N] x7;
  vector<lower=0>[N] x8;
  vector<lower=0>[N] x9;
  vector<lower=0>[N] x10;
  vector<lower=0>[N] x11;
  vector<lower=0>[N] x12;
  
}
parameters {
  real b0;
  real log_mt; // log square mt
  real rooms2_1 ; // rooms
  real rooms2_2; // rooms
  real rooms2_3; // rooms
  real rooms2_4; // rooms
  real asc; // asc
  real wc2_2; // wc
  real wc2_3; // wc
  real wc2_4; // wc
  real terraza; // rooms
  real amueblado; // rooms
  real lujo; // rooms
  
  real<lower=0> sigma_y;
}
model {
  b0 ~ cauchy(0,10);
  log_mt ~ cauchy(0,2.5);
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
  lujo ~ cauchy(0,2.5);
  
  y ~ normal(b0 + log_mt * x1 + rooms2_1 * x2 + rooms2_2 * x3 + rooms2_3 * x4 + rooms2_4 * x5 + 
  asc * x6 + wc2_2 * x7 + wc2_3 * x8 + wc2_4 * x9 + 
  terraza * x10 + amueblado * x11 + lujo * x12, sigma_y);

}
"

# util <- new.env()

model = stan_model(model_code = model_code)

# Fit the model to the data
fit_pooled <- sampling(model, data = data_list, chains = 4, iter =2000)

check_divergences(fit_pooled)
check_hmc_diagnostics(fit_pooled)

print(fit_pooled)
plot(fit_pooled)

mcmc_acf(fit_pooled)
traceplot(fit_pooled)
# Save pooled model -------------------------------------------------------

path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_pooled.RDS")

saveRDS(fit_pooled, path_to_save)

# model no pooled ---------------------------------------------------------

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
x12 <- data_cook$lujo



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
  x12 = x12,
  barri = barri
)

# radon_2 <- stan("stan_models/radon_2_no_pooled_a.stan", iter = 1000, chains = 1,
#                 data = data_list, seed = 1)



# model_code <- "
# data {
#   int<lower=0> N;
#   int<lower=0> J;
#   vector[N] y;
#   real x1[N];
#   int x2[N];
#   int barri[N];
# }
# parameters {
#   real a[J];
#   real b;
#   real c;
#   real<lower=0> sigma_y;
# }
# model {
#   for (i in 1:N)
#     y[i] ~ normal(a[barri[i]] + b * x1[i] + c * x2[i], sigma_y);
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
  int x12[N];
  int barri[N];
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
  real lujo; 
  real<lower=0> sigma_y;
}
model {
  b0 ~ cauchy(0,10);
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
  lujo ~ cauchy(0,2.5);
  for (n in 1:N)
    y[n] ~ normal(b0[barri[n]] + log_smt * x1[n] + rooms2_1 * x2[n] + 
              rooms2_2 * x3[n] + rooms2_3 * x4[n] + rooms2_4 * x5[n] + 
              asc * x6[n] + wc2_2 * x7[n] + wc2_3 * x8[n] + wc2_4 * x9[n] + 
              terraza * x10[n] + amueblado * x11[n] + lujo * x12[n], 
              sigma_y);
}
"


translate = stanc(model_code  = model_code)

model = stan_model(stanc_ret = translate)

# Fit the model to the data
fit_no_pooled <- sampling(model, data = data_list, chains = 4, iter =4000, verbose = TRUE, seed = 123) # 4000?

# fit <- stan(model_code = model_code, model_name = "no pooled", data = data_list,
#             iter = 50, chains = 1, verbose = TRUE)

# 
# ## Convergence analysis
print(fit_no_pooled)
plot(fit_no_pooled)

# mcmc_trace(fit_no_pooled)

# Save no pooled model ----------------------------------------------------

path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_no_pooled.RDS")

saveRDS(fit_no_pooled, path_to_save)

# hierarchycal model  -----------------------------------------------------


N= nrow(data_cook)
barri_name <- unique(data_cook$barri)
barri <- as.integer((data_cook$barri))
J <- length(unique(barri))
y <- data_cook$log_price
x1 <- log(data_cook$square_mt)
x2 <- data_cook$rooms

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
  x12 = x12,
  barri = barri
)

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
  int x12[N];
  int barri[N];
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
  real lujo; 
  
  real mu_a;
  real<lower=0> sigma_y;
  real<lower=0> sigma_a;
}
model {
  sigma_y ~ cauchy(0, 10);
  sigma_a ~ cauchy(0, 10);
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
  lujo ~ cauchy(0,2.5);
  b0 ~ normal(mu_a, sigma_a);            
  for (n in 1:N)
    y[n] ~ normal(b0[barri[n]] + log_smt * x1[n] + rooms2_1 * x2[n] + 
              rooms2_2 * x3[n] + rooms2_3 * x4[n] + rooms2_4 * x5[n] + 
              asc * x6[n] + wc2_2 * x7[n] + wc2_3 * x8[n] + wc2_4 * x9[n] + 
              terraza * x10[n] + amueblado * x11[n] + lujo * x12[n], sigma_y);
}
"


translate = stanc(model_code  = model_code)

model = stan_model(stanc_ret = translate)

# Fit the model to the data
fit_hier <- sampling(model, data = data_list, chains = 4, iter =5000, verbose = TRUE,seed = 1568) # 4000?

# fit <- stan(model_code = model_code, model_name = "no pooled", data = data_list,
#             iter = 50, chains = 1, verbose = TRUE)

# 
# ## Convergence analysis
print(fit_hier)
plot(fit_hier)





# Save hierarchical model -------------------------------------------------

path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_hierarchical_2.RDS")


saveRDS(fit_hier, path_to_save)
