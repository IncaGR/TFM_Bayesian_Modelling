### Test model
# We are going to test our hierarchical model with the next month data.


# read test data ----------------------------------------------------------

data_date = "2023-06-05"

path_modelling = paste0("data_lm_cook_",data_date,".RDS")

test_data<- readRDS(here::here('Desktop','1_projects','TFM','1_data','2_data_Idealista',path_modelling))

test_data$barri <- as.factor(test_data$barri)

# read fit ----------------------------------------------------------------

fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4.RDS") # this is better 842.1371
# fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_priors.RDS") # 842.204


# Extract the parameter samples from the fitted model
sims <- rstan::extract(fit)

# Generate predictions for the test data
n.sims <- nrow(sims$a)
n.test <- nrow(test_data)
y.tilde <- matrix(0, nrow = n.sims, ncol = n.test)
for (i in 1:n.test) {
  y.tilde[,i] <- rnorm(n.sims, sims$a[,test_data$barri[i]] + sims$b * log(test_data$square_mt[i]) + sims$c * test_data$rooms[i], sims$sigma_y)
}


# # Transform the predictions back to the original scale
y.tilde.exp <- exp(y.tilde)

# Compute the predicted mean price for each observation in the test data
predicted_means <- apply(y.tilde.exp, 2, mean)

# Compute the actual mean price for each observation in the test data
actual_means <- exp(test_data$log_price)

# Compute a measure of predictive performance
RMSE <- sqrt(mean((predicted_means - actual_means)^2))

plot(actual_means,predicted_means)


test_data$y_tilde = predicted_means


names(test_data)
test_data %>% ggplot(aes(x=price,y=y_tilde,color=distrito2)) + geom_jitter()

print(RMSE)
# enhance the model
