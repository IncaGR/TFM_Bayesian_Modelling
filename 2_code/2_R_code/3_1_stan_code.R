library(tidyverse)
library(car)
library(here)
library(broom)
library(broom.mixed)
library(rstan)
library(bayesplot)


# upload data ready for modelling ------------------------------------------
options(mc.cores = parallel::detectCores())
rstan_options( auto_write=TRUE )


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
fit <- sampling(model, data = data_list, chains = 2, iter =2000)


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

# Save document

# date_to_save <- str_extract(path_modelling, "\\d{4}-\\d{2}-\\d{2}")
# 
# path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/data_modeled_pooled_",date_to_save)
# 
# saveRDS(df_model,file=path_to_save)



# model no pooled ---------------------------------------------------------

N= nrow(data_cook)
barri_name <- unique(data_cook$barri)
barri <- as.integer((data_cook$barri))
J <- length(unique(barri))
y <- data_cook$log_price
x <- log(data_cook$square_mt)


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
fit <- sampling(model, data = data_list, chains = 4, iter =2000, verbose = TRUE) # 4000?

# fit <- stan(model_code = model_code, model_name = "no pooled", data = data_list,
#             iter = 50, chains = 1, verbose = TRUE)

# 
# ## Convergence analysis
print(fit)
plot(fit)

price_summary <- tidy(fit, conf.int = T, level = 0.8, rhat = T, ess = T)

df_no_pooled  <- tibble(
  barri = barri_name,
  model = "no_pooled",
  intercept = price_summary$estimate[1:J],
  slope = price_summary$estimate[J+1]
)

id_barrio<- c("la Guineueta",
               "la Vall d'Hebron", "Canyelles", "la Trinitat Nova", "el Raval", "la Dreta de l'Eixample", "el Barri Gòtic",
               "Sants")

df_model <- bind_rows(df_pooled, df_no_pooled) %>%
# df_model <- df_no_pooled %>% 
  left_join(data_cook, by = "barri") %>%
  filter(barri %in% id_barrio) 

ggplot(df_model) +
  geom_jitter(aes(log(square_mt), log_price)) +
  geom_abline(aes(intercept = intercept, slope = slope, color = model)) +
  facet_wrap(~ barri, ncol = 4) + 
  scale_x_continuous(breaks = 0:1) + 
  theme(legend.position = "bottom")


# hierarchycal model  -----------------------------------------------------


