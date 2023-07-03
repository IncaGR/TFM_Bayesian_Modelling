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
x <- log(data_cook$square_mt)
arbres <- data_cook %>%
  group_by(barri) %>%
  summarise(arbres = first(log(n_arbres_bcn_barri))) %>%
  pull(arbres) 

data_list <- list(
  N = N,
  J = J,
  y = y,
  x = x,
  barri = barri,
  arbres = arbres
)

model_code <- "
data {
  int<lower=0> N; 
  int<lower=0> J;
  vector[N] y;
  real x[N];
  int barri[N];
  vector[J] arbres;
}
parameters {
  real a[J];
  real b;                           
  real g_0;
  real g_1;
  real<lower=0> sigma_y;
  real<lower=0> sigma_a;
}
model {
  for (j in 1:J)
    a[j] ~ normal(g_0 + g_1 * arbres[j], sigma_a);
  for (n in 1:N)
    y[n] ~ normal(a[barri[n]] + b * x[n], sigma_y);
}
"

translate = stanc(model_code  = model_code)

model = stan_model(stanc_ret = translate)

# Fit the model to the data
fit_4 <- sampling(model, data = data_list, chains = 4, iter =4000, verbose = TRUE) # 4000?

# ## Convergence analysis
print(fit_4)
plot(fit_4)

# Save hierarchical model -------------------------------------------------

path_to_save = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4.RDS")


saveRDS(fit_4, path_to_save)


# predicting new unit -----------------------------------------------------


sims <- rstan::extract(fit_4)

# new unit in an existing group 
a <- sims$a
b <- sims$b
sigma.y <- sims$sigma_y
n.sims <- dim(a)[1]
y.tilde <- rnorm(n.sims, a[,1] + b * log(75), sigma.y)

exp(y.tilde)
hist(y.tilde)
hist(exp(y.tilde))
mean(exp(y.tilde))

