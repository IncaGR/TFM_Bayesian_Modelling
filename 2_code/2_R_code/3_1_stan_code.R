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

N= nrow(data_cook)
barri_name <- unique(data_cook$barri)
barri <- as.numeric(data_cook$barri)
J <- length(unique(barri))
y <- data_cook$log_price
x1 <- log(data_cook$square_mt)
x2 <- data_cook$rooms



data_list <- list(
  N = N,
  y = y,
  x1 = x1,
  x2 = x2
)

model_code <- "
data {
  int<lower=0> N;
  vector[N] y;
  vector<lower=0>[N] x1;
  vector<lower=0>[N] x2;
}
parameters {
  real a;
  real b;
  real c;
  real<lower=0> sigma_y;
}
model {
  y ~ normal(a + b * x1 + c * x2, sigma_y);
}
"

# util <- new.env()

model = stan_model(model_code = model_code)

# Fit the model to the data
fit_pooled <- sampling(model, data = data_list, chains = 2, iter =2000)

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
barri <- as.integer((data_cook$barri))
J <- length(unique(barri))
y <- data_cook$log_price
x1 <- log(data_cook$square_mt)
x2 <- data_cook$rooms

data_list <- list(
  N = N,
  J = J,
  y = y,
  x1 = x1,
  x2 = x2,
  barri = barri
)

# radon_2 <- stan("stan_models/radon_2_no_pooled_a.stan", iter = 1000, chains = 1,
#                 data = data_list, seed = 1)

model_code <- "
data {
  int<lower=0> N;
  int<lower=0> J;
  vector[N] y;
  real x1[N];
  int x2[N];
  int barri[N];
}
parameters {
  real a[J];
  real b;
  real c;
  real<lower=0> sigma_y;
}
model {
  for (i in 1:N)
    y[i] ~ normal(a[barri[i]] + b * x1[i] + c * x2[i], sigma_y);
}
"


translate = stanc(model_code  = model_code)

model = stan_model(stanc_ret = translate)

# Fit the model to the data
fit_no_pooled <- sampling(model, data = data_list, chains = 4, iter =4000, verbose = TRUE) # 4000?

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
x1 <- log(data_cook$square_mt)
x2 <- data_cook$rooms

data_list <- list(
  N = N,
  J = J,
  y = y,
  x1 = x1,
  x2 = x2,
  barri = barri
)

model_code <- "
data {
  int<lower=0> N;
  int<lower=0> J;
  vector[N] y;
  real x1[N];
  int x2[N];
  int barri[N];
}
parameters {
  real a[J];
  real b;
  real c;
  real mu_a;
  real<lower=0> sigma_y;
  real<lower=0> sigma_a;
}
model {
  a ~ normal(mu_a, sigma_a);            
  for (n in 1:N)
    y[n] ~ normal(a[barri[n]] + b * x1[n] + c * x2[n], sigma_y);
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
