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

path_modelling = paste0("data_lm_cook_",data_date)

data_cook<- readRDS(here::here('Desktop','1_projects','TFM','1_data','2_data_Idealista',path_modelling))

data_cook$barri <- as.factor(data_cook$barri)


# price_pooled ------------------------------------------------------------

N= nrow(data_cook)
barri <- as.numeric(data_cook$id_barri)
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
fit_pooled <- sampling(model, data = data_list, chains = 2, iter =2000)


print(fit_pooled)
plot(fit_pooled)


# Save pooled model -------------------------------------------------------

path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_pooled.RDS")

saveRDS(fit_pooled, path_to_save)

# model no pooled ---------------------------------------------------------

N= nrow(data_cook)
barri_name <- unique(data_cook$barri)
barri <- as.integer((data_cook$barri))
J <- length(unique(barri))
y <- data_cook$log_price
x <- log(data_cook$square_mt)

data_list <- list(
  N = N,
  J = J,
  y = y,
  x = x,
  barri = barri
)

# radon_2 <- stan("stan_models/radon_2_no_pooled_a.stan", iter = 1000, chains = 1,
#                 data = data_list, seed = 1)

model_code <- "
data {
  int<lower=0> N;
  int<lower=0> J;
  vector[N] y;
  real x[N];
  int barri[N];
}
parameters {
  real a[J];
  real b;              
  real<lower=0> sigma_y;
}
model {
  for (i in 1:N)
    y[i] ~ normal(a[barri[i]] + b * x[i], sigma_y);
}

"


translate = stanc(model_code  = model_code)

model = stan_model(stanc_ret = translate)

# Fit the model to the data
fit_no_pooled <- sampling(model, data = data_list, chains = 4, iter =2000, verbose = TRUE) # 4000?

# fit <- stan(model_code = model_code, model_name = "no pooled", data = data_list,
#             iter = 50, chains = 1, verbose = TRUE)

# 
# ## Convergence analysis
print(fit_no_pooled)
plot(fit_no_pooled)

mcmc_trace(fit_no_pooled)

# Save no pooled model ----------------------------------------------------

path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_no_pooled.RDS")

saveRDS(fit_no_pooled, path_to_save)

# hierarchycal model  -----------------------------------------------------


N= nrow(data_cook)
barri_name <- unique(data_cook$barri)
barri <- as.integer((data_cook$barri))
J <- length(unique(barri))
y <- data_cook$log_price
x <- log(data_cook$square_mt)

data_list <- list(
  N = N,
  J = J,
  y = y,
  x = x,
  barri = barri
)

model_code <- "
data {
  int<lower=0> N;
  int<lower=0> J;
  vector[N] y;
  real x[N];
  int barri[N];
}
parameters {
  real a[J];
  real b;                           
  real mu_a;
  real<lower=0> sigma_y;
  real<lower=0> sigma_a;
}
model {
  a ~ normal(mu_a, sigma_a);            
  for (n in 1:N)
    y[n] ~ normal(a[barri[n]] + b * x[n], sigma_y);
}
"


translate = stanc(model_code  = model_code)

model = stan_model(stanc_ret = translate)

# Fit the model to the data
fit_hier <- sampling(model, data = data_list, chains = 4, iter =2000, verbose = TRUE) # 4000?

# fit <- stan(model_code = model_code, model_name = "no pooled", data = data_list,
#             iter = 50, chains = 1, verbose = TRUE)

# 
# ## Convergence analysis
print(fit_hier)
plot(fit_hier)


# Save hierarchical model -------------------------------------------------

path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_hierarchical.RDS")


saveRDS(fit_hier, path_to_save)
