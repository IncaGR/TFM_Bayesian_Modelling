### Test model
# We are going to test our hierarchical model with the next month data.
library(dplyr)
library(ggplot2)
library(rstan)
library(here)
library(broom.mixed)

# read test data ----------------------------------------------------------

set.seed(125)

# para las predicciones vamos a comprar r-2 y RMSE de los 4 modelos.
# Bayesian pooled, no pooled, hierarchical y hierarchical covariable.

data_date = "2023-06-05"

path_modelling = paste0("data_lm_cook_",data_date,".RDS")

test_data<- readRDS(here::here('1_data','2_data_Idealista',path_modelling))

test_data$barri <- as.factor(test_data$barri)

table(test_data$barri)

test_data = test_data[!is.na(test_data$price),]

summary(test_data)

y <- exp(test_data$log_price)

n<-nrow(test_data)


# test_data = test_data %>% filter(lujo == 0)

# read fit ----------------------------------------------------------------

# fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_pooled.RDS") # pooled 1325.239, 0.5236905
# fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_no_pooled.RDS") # no pooled 1435.512

# fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_hierarchical_2.RDS") # 1385.68, 0.479253

# fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4.RDS") # this is better 842.1371
# fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_priors.RDS") # 842.204
# fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_1.RDS") # 800.5172 
# fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_2.RDS") # 797.9225
# fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_2_1.RDS") # 798.4418
# fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_3.RDS") # no lujo 586.7039
# fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_4.RDS") # 581.281 # Best model 0.7019824
# fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_5.RDS") # con playa
# fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_6.RDS") # 1050.08, 0.700949
# fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_8.RDS") # 662.9664 No tiene lujo


# fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_9.RDS")
# parece que lujo esta muy correlacionada con otras variables, esto hace que el std.error sea muy elevado y al calcular 
# rsquared me da infnito porque el coeficiente de lujo a veces da +-200.
# probar quitar lujo
# faltaba lujo ahora es el mejor modelo: 0.7114481, 1029.92

# try variant in intercept and slope
# try only no lujo observations

# fit
# summary(fit)

# hier_1_tidy =  tidy(fit) 


# save_tidy = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/hier_1_tidy.RDS")

# saveRDS(hier_1_tidy,file=save_tidy)

# Extract the parameter samples from the fitted model

# Create df

df_metrics = data.frame(model = c('lm','pooled','no_pooled','hierarchical','hierarchical_cov'), 
                        r_2 = c(0,0,0,0,0),r_2_adj = c(0,0,0,0,0),rmse = c(0,0,0,0,0)
                        ,wrmse = c(0,0,0,0,0))

# test wrmse

# Sample data: Replace this with your actual data
# predicted <- c(1, 2, 3, 4, 5)
# actual <- c(1.2, 1.8, 3.5, 3.8, 5.2)
# neighborhood <- c('A', 'A', 'B', 'B', 'B')
# 
# # Compute the weights
# weights <- 1 / table(neighborhood)
# obs_weights <- weights[neighborhood]
# 
# # Compute the weighted RMSE
# wrmse <- sqrt(sum(obs_weights * (actual - predicted)^2) / sum(obs_weights))
# wrmse

test_data$id_barri

weights <- 1 / table(test_data$id_barri)

obs_weights <- weights[test_data$id_barri]

# prediction of the pooles model ------------------------------------------

POOLED = FALSE

if(POOLED){   
  
  fit <- readRDS("./1_data/2_data_Idealista/3_fitted_data/model_pooled.RDS")
  
  df = tidyMCMC(fit) 
  k = df %>% filter(!grepl("^b0|sigma_y|lp_",df$term)) %>% nrow()
  
  sims <- rstan::extract(fit)
  
  y_pred = mean(sims$b0) + 
    mean(sims$log_mt) * log(test_data$square_mt) + 
    mean(sims$rooms2_1) * test_data$rooms2_1 +
    mean(sims$rooms2_2) * test_data$rooms2_2 +
    mean(sims$rooms2_3) * test_data$rooms2_3 +
    mean(sims$rooms2_4) * test_data$rooms2_4 +
    mean(sims$asc) * test_data$asc +
    mean(sims$wc2_2) * test_data$wc2_2 +
    mean(sims$wc2_3) * test_data$wc2_3 +
    mean(sims$wc2_4) * test_data$wc2_4 +
    mean(sims$terraza) * test_data$terraza +
    mean(sims$amueblado) * test_data$amueblado +
    mean(sims$lujo) * test_data$lujo 
  
  
  
  y_pred = exp(y_pred)
  y <- exp(test_data$log_price)
  
  
  # Compute a measure of predictive performance
  RMSE <- sqrt(mean((y_pred - y)^2))
  
  wrmse <- sqrt(sum(obs_weights * (y - y_pred)^2,na.rm= T) / sum(obs_weights,na.rm = T))
  
  print(RMSE)
  
  rsquared = 1 - (sum((y - y_pred)^2)/sum((y - mean(y))^2))
  
  adj_rsquared = 1 - (1-rsquared)*((n-1)/(n-k-1))
  
  print(adj_rsquared)
  print(rsquared)
  
  df_metrics[df_metrics$model == 'pooled',]$r_2 = round(rsquared, 3)
  df_metrics[df_metrics$model == 'pooled',]$rmse = round(RMSE, 3)
  df_metrics[df_metrics$model == 'pooled',]$r_2_adj = round(adj_rsquared, 3)
  df_metrics[df_metrics$model == 'pooled',]$wrmse = round(wrmse, 3)
  
  print(df_metrics)
  }


# predictions no pooled ---------------------------------------------------

NO_POOLED = FALSE

if(NO_POOLED){
  
  fit <- readRDS("./1_data/2_data_Idealista/3_fitted_data/model_no_pooled.RDS") # no pooled 1435.512
  df = tidyMCMC(fit) 
  k = df %>% filter(!grepl("sigma_y|lp_",df$term)) %>% nrow()
  
  sims <- rstan::extract(fit)
  
  n.sims <- nrow(sims$b0)
  n.test <- nrow(test_data)
  y.tilde <- matrix(0, nrow = n.sims, ncol = n.test)
  for (i in 1:n.test) {
    # print(i)
    y.tilde[,i] <- rnorm(n.sims, sims$b0[,test_data$barri[i]] 
                         + sims$log_smt * test_data$log_smt[i]
                         + sims$rooms2_1 * test_data$rooms2_1[i]
                         + sims$rooms2_2 * test_data$rooms2_2[i]
                         + sims$rooms2_3 * test_data$rooms2_3[i]
                         + sims$rooms2_4 * test_data$rooms2_4[i]
                         + sims$asc * test_data$asc[i]
                         + sims$wc2_2 * test_data$wc2_2[i]
                         + sims$wc2_3 * test_data$wc2_3[i]
                         + sims$wc2_4 * test_data$wc2_4[i]
                         + sims$terraza * test_data$terraza[i]
                         + sims$amueblado * test_data$amueblado[i]
                         + sims$lujo * test_data$lujo[i]
                         , sims$sigma_y)
  }
  
  y.tilde.exp <- exp(y.tilde)
  
  # Compute the predicted mean price for each observation in the test datahttp://127.0.0.1:36221/graphics/plot_zoom_png?width=2195&height=1182
  predicted_means <- apply(y.tilde.exp, 2, mean)
  
  # Compute the actual mean price for each observation in the test data
  actual_means <- exp(test_data$log_price)
  
  RMSE <- sqrt(mean((predicted_means - actual_means)^2))
  
  print(RMSE)
  
  rsquared = 1 - (sum((actual_means - predicted_means)^2)/sum((actual_means - mean(actual_means))^2))
  wrmse <- sqrt(sum(obs_weights * (actual_means - predicted_means)^2,na.rm= T) / sum(obs_weights,na.rm = T))
  
  # 1 - (1 - R.squared) * ((n - 1)/(n-p-1))
  
  adj_rsquared = 1 - (1-rsquared)*((n-1)/(n-k-1))
  
  print(adj_rsquared)
  print(rsquared)
  
  df_metrics[df_metrics$model == 'no_pooled',]$r_2 = round(rsquared, 3)
  df_metrics[df_metrics$model == 'no_pooled',]$rmse = round(RMSE, 3)
  df_metrics[df_metrics$model == 'no_pooled',]$r_2_adj = round(adj_rsquared, 3)
  df_metrics[df_metrics$model == 'no_pooled',]$wrmse = round(wrmse, 3)
  
  print(df_metrics)
}


# modelo jerarquico barrio ------------------------------------------------

HIER = FALSE

if(HIER){
  
  fit <- readRDS("./1_data/2_data_Idealista/3_fitted_data/model_hierarchical_2.RDS")
  df = tidyMCMC(fit) 
  
  k = df %>% filter(!grepl("sigma_y|lp_|mu_a|sigma_a",df$term)) %>% nrow()
  
  sims <- rstan::extract(fit)
  
  # broom.mixed::tidy(sims)
  
  
  n.sims <- nrow(sims$b0)
  n.test <- nrow(test_data)
  y.tilde <- matrix(0, nrow = n.sims, ncol = n.test)
  for (i in 1:n.test) {
    # print(i)
    y.tilde[,i] <- rnorm(n.sims, sims$b0[,test_data$barri[i]] 
                         + sims$log_smt * log(test_data$square_mt[i]) 
                         + sims$rooms2_1 * test_data$rooms2_1[i]
                         + sims$rooms2_2 * test_data$rooms2_2[i]
                         + sims$rooms2_3 * test_data$rooms2_3[i]
                         + sims$rooms2_4 * test_data$rooms2_4[i]
                         + sims$asc * test_data$asc[i]
                         + sims$wc2_2 * test_data$wc2_2[i]
                         + sims$wc2_3 * test_data$wc2_3[i]
                         + sims$wc2_4 * test_data$wc2_4[i]
                         + sims$terraza * test_data$terraza[i]
                         + sims$amueblado * test_data$amueblado[i]
                         + sims$lujo * test_data$lujo[i]
                         , sims$sigma_y)
  }
  
  y.tilde.exp <- exp(y.tilde)
  
  # Compute the predicted mean price for each observation in the test datahttp://127.0.0.1:36221/graphics/plot_zoom_png?width=2195&height=1182
  predicted_means <- apply(y.tilde.exp, 2, mean)
  
  # Compute the actual mean price for each observation in the test data
  actual_means <- exp(test_data$log_price)
  
  
  RMSE <- sqrt(mean((predicted_means - actual_means)^2))
  
  wrmse <- sqrt(sum(obs_weights * (predicted_means - actual_means)^2,na.rm= T) / sum(obs_weights,na.rm = T))
  
  print(RMSE)
  
  rsquared = 1 - (sum((actual_means - predicted_means)^2)/sum((actual_means - mean(actual_means))^2))
  adj_rsquared = 1 - (1-rsquared)*((n-1)/(n-k-1))
  
  print(rsquared)
  
  df_metrics[df_metrics$model == 'hierarchical',]$r_2 = round(rsquared, 3)
  df_metrics[df_metrics$model == 'hierarchical',]$rmse = round(RMSE, 3)
  df_metrics[df_metrics$model == 'hierarchical',]$r_2_adj = round(adj_rsquared, 3)
  df_metrics[df_metrics$model == 'hierarchical',]$wrmse = round(wrmse, 3)
  
  print(df_metrics)
}





# Resto de moldelos jerarquicos -------------------------------------------


HIER_COV = TRUE

if(HIER_COV){
  
  fit <- readRDS("./1_data/2_data_Idealista/3_fitted_data/model_4_9.RDS")
  df = tidyMCMC(fit) 
  
  k = df %>% filter(!grepl("sigma_y|lp_|mu_a|sigma_a|g_0|g_1",df$term)) %>% nrow()
  
  sims <- rstan::extract(fit)  
  
  # Generate predictions for the test data
  n.sims <- nrow(sims$b0)
  n.test <- nrow(test_data)
  y.tilde <- matrix(0, nrow = n.sims, ncol = n.test)
  
  for (i in 1:n.test) {
    # print(i) # debug
    y.tilde[,i] <- rnorm(n.sims, sims$b0[,test_data$barri[i]] 
                         + sims$log_smt * log(test_data$square_mt[i]) 
                         + sims$rooms2_1 * test_data$rooms2_1[i]
                         + sims$rooms2_2 * test_data$rooms2_2[i]
                         + sims$rooms2_3 * test_data$rooms2_3[i]
                         + sims$rooms2_4 * test_data$rooms2_4[i]
                         + sims$asc * test_data$asc[i]
                         + sims$wc2_2 * test_data$wc2_2[i]
                         + sims$wc2_3 * test_data$wc2_3[i]
                         + sims$wc2_4 * test_data$wc2_4[i]
                         + sims$terraza * test_data$terraza[i]
                         + sims$amueblado * test_data$amueblado[i]
                         + sims$lujo * test_data$lujo[i]
                         , sims$sigma_y)
  }
  
  # # Transform the predictions back to the original scale
  y.tilde.exp <- exp(y.tilde)
  
  # Compute the predicted mean price for each observation in the test datahttp://127.0.0.1:36221/graphics/plot_zoom_png?width=2195&height=1182
  predicted_means <- apply(y.tilde.exp, 2, mean)
  
  # Compute the actual mean price for each observation in the test data
  actual_means <- exp(test_data$log_price)
  
  # Compute a measure of predictive performance
  RMSE <- sqrt(mean((predicted_means - actual_means)^2))
  
  wrmse <- sqrt(sum(obs_weights * (actual_means - predicted_means)^2,na.rm= T) / sum(obs_weights,na.rm = T))
  
  print(RMSE)
  
  rsquared = 1 - (sum((actual_means - predicted_means)^2)/sum((actual_means - mean(actual_means))^2))
  adj_rsquared = 1 - (1-rsquared)*((n-1)/(n-k-1))
  
  print(rsquared)
  
  df_metrics[df_metrics$model == 'hierarchical_cov',]$r_2 = round(rsquared, 3)
  df_metrics[df_metrics$model == 'hierarchical_cov',]$rmse = round(RMSE, 3)
  df_metrics[df_metrics$model == 'hierarchical_cov',]$r_2_adj = round(adj_rsquared, 3)
  df_metrics[df_metrics$model == 'hierarchical_cov',]$wrmse = round(wrmse, 3)
  
  print(df_metrics)
  
}


# aqui ya hemos elegido el modelo hier con covariable
# ahora quiero ver la forma de hacer la predicciones 1 a 1 
# para poder implementarlas en una tabla en shiny

mpaping = here("1_data","2_data_Idealista","mapping_barri_coeff.rds")
  
mapping = readRDS(mpaping)


# Define values for the predictors
neighborhood_value <- 9
rooms2_2 <- 0
rooms2_3 <- 0
rooms2_4 <- 1
wc2_2 <- 1
wc2_4 <- 0
terraza <- 1
asc <- 1
smt <- 90
lujo <- 0
amueblado <- 0


# Extract the posterior samples for coefficients (assuming your model has a linear structure)
 # Assuming you have a variable named 'b0' in your model

# Initialize an empty vector to store the linear predictions


linear_predictions <- numeric(n.sims)

# Loop through each posterior sample to calculate the linear prediction
for (i in 1:n.sims) {
  linear_predictions[i] <- b0_samples[i, neighborhood_value] +
    rooms2_3 * sims$rooms2_3[i] + 
    rooms2_4 * sims$rooms2_4[i] +
    wc2_2 * sims$wc2_2[i] +
    terraza * sims$terraza[i] +
    asc * sims$asc[i] +
    log(smt) * sims$log_smt[i] +
    lujo * sims$lujo[i] +
    amueblado * sims$amueblado[i]
}

# Assuming your model involves log-transformed price
predicted_prices <- exp(linear_predictions)

# Calculate summary statistics
mean_predicted_price <- mean(predicted_prices)
median_predicted_price <- median(predicted_prices)
credible_interval <- quantile(predicted_prices, c(0.025, 0.975))


print(mean_predicted_price)

hist(predicted_prices)






# TODO: guardar tabla de metricas y pasar a formato latex

# LATEX:

# \begin{tabular}{lccc}
# \hline
# model           & $r^2$  & rmse    \\
# \hline
# pooled          & 0.746  & 965.386 \\
# no\_pooled      & 0.706  & 1039.901\\
# hierarchical    & 0.623  & 1176.690\\
# hierarchical\_cov & 0.712  & 1029.722\\
# \hline
# \end{tabular}



# TODO: añadir modelo lineal a la tabla
# TODO: mover a viz

# plot(actual_means,predicted_means)
# 
# 
# test_data$y_tilde = predicted_means
# 
# 
# names(test_data)
# test_data %>% ggplot(aes(x=price,y=y_tilde,color = as.factor(lujo))) + geom_jitter(alpha=0.5,shape = 1) +
#   facet_wrap(vars(distrito2)) +
#   theme_bw()
# 
# 
# print(RMSE)



# jerarquico rooms y wc binarias ------------------------------------------

# test_data = test_data %>% filter(barri != "Vilapicina i la Torre Llobeta")

# # Generate predictions for the test data
# n.sims <- nrow(sims$b0)
# n.test <- nrow(test_data)
# y.tilde <- matrix(0, nrow = n.sims, ncol = n.test)
# for (i in 1:n.test) {
#   print(i)
#   y.tilde[,i] <- rnorm(n.sims, sims$b0[,test_data$barri[i]] + sims$log_smt * log(test_data$square_mt[i])  
#                        + sims$rooms2_1 * test_data$rooms2_1[i]
#                        + sims$rooms2_2 * test_data$rooms2_2[i]
#                        + sims$rooms2_3 * test_data$rooms2_3[i]
#                        + sims$rooms2_4 * test_data$rooms2_4[i]
#                        + sims$wc2_2 * test_data$wc2_2[i]
#                        + sims$wc2_3 * test_data$wc2_3[i]
#                        + sims$wc2_4 * test_data$wc2_4[i]
#                        + sims$asc * test_data$asc[i] 
#                        + sims$terraza * test_data$terraza[i]
#                        + sims$amueblado * test_data$amueblado[i]
#                        + sims$lujo * test_data$lujo[i]
#                        , sims$sigma_y)
# }
# 
# # # Transform the predictions back to the original scale
# y.tilde.exp <- exp(y.tilde)
# 
# # Compute the predicted mean price for each observation in the test datahttp://127.0.0.1:36221/graphics/plot_zoom_png?width=2195&height=1182
# predicted_means <- apply(y.tilde.exp, 2, mean)
# 
# # Compute the actual mean price for each observation in the test data
# actual_means <- exp(test_data$log_price)
# 
# # Compute a measure of predictive performance
# RMSE <- sqrt(mean((predicted_means - actual_means)^2))
# 
# print(RMSE)
# 
# rsquared = 1 - (sum((actual_means - predicted_means)^2)/sum((actual_means - mean(actual_means))^2))
# 
# print(rsquared)

# predict linear model -----------------------------------------------------

# if (!(data_predict != data_date & MAKE_PREDICTIONS)){
#   stop("Modelling and predicting same dataset or MAKE_PREDICTIONS equal FALSE")
# }

# path_predict = paste0("data_modelling_",data_predict,".RDS")
# path_predict = paste0("data_lm_cook_",data_predict,".RDS")

# predict_sample <- readRDS(here::here('1_data','2_data_Idealista',path_predict))

# table(predict_sample$wc2)
# table(predict_sample$wc)

# predict_sample$rooms2 <- as.factor(predict_sample$rooms2)

# predict_sample = predict_sample %>% filter(lujo == 0)

lm_cook = readRDS("./1_data/2_data_Idealista/model_cook_2023-05-03.RDS")

# unique(test_data$barri)
k = length(lm_cook$coefficients)
# lm_cook
# test_data %>% filter(barri == "Ciutat Meridiana") 
test_data = test_data %>% filter(barri != "Ciutat Meridiana") # removing Ciutat Meridiana



predictions = exp(predict.lm(lm_cook,test_data))


# Compute the actual mean price for each observation in the test data
real_values <- exp(test_data$log_price)

# Compute a measure of predictive performance
RMSE <- sqrt(mean((predictions - real_values)^2))
print(RMSE)
wrmse <- sqrt(sum(obs_weights * (predictions - real_values)^2,na.rm= T) / sum(obs_weights,na.rm = T))

plot(real_values,predictions)

# predict_sample$y_tilde = predictions

# names(predict_sample)

rsquared = 1 - (sum((real_values - predictions)^2)/sum((real_values - mean(real_values))^2))
adj_rsquared = 1 - (1-rsquared)*((n-1)/(n-k-1))

print(rsquared)

df_metrics[df_metrics$model == 'lm',]$r_2 = round(rsquared, 3)
df_metrics[df_metrics$model == 'lm',]$rmse = round(RMSE, 3)
df_metrics[df_metrics$model == 'lm',]$r_2_adj = round(adj_rsquared, 3)
df_metrics[df_metrics$model == 'lm',]$wrmse = round(wrmse, 3)

print(df_metrics)


# TODO: probar varibles standarizadas...
# https://projecteuclid.org/journals/annals-of-applied-statistics/volume-2/issue-4/A-weakly-informative-default-prior-distribution-for-logistic-and-other/10.1214/08-AOAS191.full

# TODO: escribir modelos bayesianos
# TODO: escribir resultados y metricas
# TODO: prototipo de shiny app
# TODO: explicar shiny app, proposito y mostrar ejemplo de prediccion
