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


names(data_cook)
# price hierarchical covariate --------------------------------------------

N= nrow(data_cook)
barri_name <- unique(data_cook$barri)
barri <- as.integer((data_cook$barri))
J <- length(unique(barri))
y <- data_cook$log_price
x1 <- log(data_cook$square_mt)
# x1 <- data_cook$square_mt # test no log NO USE TAKE MUCH TIME
x2 <- data_cook$rooms
# x2 <- data_cook$rooms2 # test no working only numeric vals
x3 <- data_cook$wc # test other wc2, wcx
x4 <- data_cook$lujo
mean_income <- data_cook %>%
group_by(barri) %>%
summarise(mean_income = first(log(mean_income))) %>%
pull(mean_income)
# mean_income <- data_cook %>%
#   group_by(barri) %>%
#   summarise(mean_income = first(mean_income)) %>% # test no log
#   pull(mean_income) 

data_list <- list(
  N = N,
  J = J,
  y = y,
  # x = x,
  x1 = x1, # log square mt
  x2 = x2, # nº rooms
  x3 = x3, # nº wc
  x4 = x4, # lujo
  barri = barri,
  mean_income = mean_income
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
  int barri[N];
  vector[J] mean_income;
}
parameters {
  real a[J]; 
  real b;
  real c;
  real d;
  real e;
  real g_0;
  real g_1;
  real<lower=0> sigma_y;
  real<lower=0> sigma_a;
}
model {

  sigma_y ~ cauchy(0, 10);
  sigma_a ~ cauchy(0, 10);
  b ~ cauchy(0,2.5);
  c ~ cauchy(0,2.5);
  d ~ cauchy(0,2.5);
  e ~ cauchy(0,2.5);
  for (j in 1:J)
    a[j] ~ normal(g_0 + g_1 * mean_income[j], sigma_a);
  for (n in 1:N)
    y[n] ~ normal(a[barri[n]] + b * x1[n] + c * x2[n] + d * x3[n] + e * x4[n], sigma_y);
}
"

# PRIORS (does not coverge well)
# model_code <- "
# data {
#   int<lower=0> N; 
#   int<lower=0> J;
#   vector[N] y;
#   real x1[N];
#   int x2[N];
#   int barri[N];
#   vector[J] mean_income;
# }
# parameters {
#   real a[J];
#   real b;  
#   real c;
#   real g_0;
#   real g_1;
#   real<lower=0> sigma_y;
#   real<lower=0> sigma_a;
# }
# model {
#   g_0 ~ normal(0, 10);
#   g_1 ~ normal(0, 10);
#   sigma_y ~ cauchy(0, 10);
#   sigma_a ~ cauchy(0, 10);
#   b ~ cauchy(0, 10);
#   c ~ cauchy(0, 10);
#   
#   for (j in 1:J)
#     a[j] ~ normal(g_0 + g_1 * mean_income[j], sigma_a);
#   for (n in 1:N)
#     y[n] ~ normal(a[barri[n]] + b * x1[n] + c * x2[n], sigma_y);
# 
# }
# "

translate = stanc(model_code  = model_code)

model = stan_model(stanc_ret = translate)

# Fit the model to the data
fit_4 <- sampling(model, data = data_list, chains = 4, iter =6000, verbose = TRUE, seed = 132) # 4000?

# ## Convergence analysis
print(fit_4)
plot(fit_4)

# Save hierarchical model -------------------------------------------------

# path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4.RDS")
# path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_no_log.RDS") # test no log square mt
# path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_priors.RDS") # with priors
# path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_1.RDS") # wc cov
# path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_2.RDS") # terrace
path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_2_1.RDS") # lujo cauchy narrow


saveRDS(fit_4, path_to_save)



# read fit ----------------------------------------------------------------

fit_4 <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4.RDS")

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
sigma.y <- sims$sigma_y
n.sims <- nrow(a)
y.tilde <- rnorm(n.sims, a[,40] + b * log(90) + c * 3 + d * 2 + e * 0, sigma.y)

# exp(y.tilde)
hist(y.tilde)
hist(exp(y.tilde))
mean(exp(y.tilde))

plot(fit_4)
plot(fit_4,plotfun = "rhat")
plot(fit_4, plotfun = "trace", pars = c("b", "c","d","e"), inc_warmup = TRUE)
plot(fit_4, show_density = TRUE, pars=c("b","c","d","e"), ci_level = 0.8, fill_color = "purple")



# model diagnostic --------------------------------------------------------
