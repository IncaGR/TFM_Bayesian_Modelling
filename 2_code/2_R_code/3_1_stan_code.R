library(tidyverse)
library(car)
library(here)
library(broom)
library(broom.mixed)
library(rstan)
library(bayesplot)


# upload data ready for modelling ------------------------------------------

data_date = "2023-05-03"

path_modelling = paste0("data_modeled_pooled_",data_date)

data_pooled <- readRDS(here::here('Desktop','1_projects','TFM','1_data','2_data_Idealista',path_modelling))

N= nrow(data_pooled)
barri_name <- as.factor(unique(data_pooled$barri))
barri <- as.numeric(as.factor(data_pooled$barri))
J <- length(unique(barri))
y <- data_pooled$log_price
x <- log(data_pooled$square_mt)


# price no pooled ---------------------------------------------------------

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
  vector<lower=0>[N] x;
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

# model_2 = stan_model(model_code = model_code)
# 
# # Fit the model to the data
# tempdir()
# fit_2 <- sampling(model_2, data = data_list, chains = 1, iter =100)
# 
# ## Convergence analysis
# print(fit_2)
# plot(fit_2)

fit <- stan(model_code = model_code, model_name = "no pooled", data = data_list,
            iter = 50, chains = 1, verbose = TRUE)

# 
# ## Convergence analysis
# print(fit)
# plot(fit)

price_summary <- tidy(fit_2, conf.int = T, level = 0.8, rhat = T, ess = T)

df_no_pooled  <- tibble(
  county = barri_name,
  model = "no_pooled",
  intercept = price_summary$estimate[1:J],
  slope = price_summary$estimate[J+1]
)

id_barrio<- c("la Guineueta",
               "la Vall d'Hebron", "Canyelles", "la Trinitat Nova", "el Raval", "la Dreta de l'Eixample", "el Barri Gòtic",
               "Sants")

df_model <- bind_rows(df_pooled, df_no_pooled) %>% 
  left_join(data_cook, by = "barri") %>%
  filter(barri %in% id_barrio) 

ggplot(df_model) +
  geom_jitter(aes(log(square_mt), log_price)) +
  geom_abline(aes(intercept = intercept, slope = slope, color = model)) +
  facet_wrap(~ barri, ncol = 4) + 
  scale_x_continuous(breaks = 0:1) + 
  theme(legend.position = "bottom")

