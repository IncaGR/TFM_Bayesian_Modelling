### Test model
# We are going to test our hierarchical model with the next month data.


# read test data ----------------------------------------------------------

data_date = "2023-06-05"

path_modelling = paste0("data_lm_cook_",data_date,".RDS")

test_data<- readRDS(here::here('Desktop','1_projects','TFM','1_data','2_data_Idealista',path_modelling))

test_data$barri <- as.factor(test_data$barri)

test_data = test_data %>% filter(lujo == 0)

# read fit ----------------------------------------------------------------

# fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4.RDS") # this is better 842.1371
# fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_priors.RDS") # 842.204
# fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_1.RDS") # 800.5172 
# fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_2.RDS") # 797.9225
fit <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_4_2_1.RDS") # 798.4418

summary(fit)
# Extract the parameter samples from the fitted model
sims <- rstan::extract(fit)

# Generate predictions for the test data
n.sims <- nrow(sims$a)
n.test <- nrow(test_data)
y.tilde <- matrix(0, nrow = n.sims, ncol = n.test)
for (i in 1:n.test) {
  y.tilde[,i] <- rnorm(n.sims, sims$a[,test_data$barri[i]] + sims$b * log(test_data$square_mt[i]) + sims$c * test_data$rooms[i]
                       + sims$d * test_data$wc[i] + sims$e * test_data$lujo[i], sims$sigma_y)
}

# # Transform the predictions back to the original scale
y.tilde.exp <- exp(y.tilde)

# Compute the predicted mean price for each observation in the test datahttp://127.0.0.1:36221/graphics/plot_zoom_png?width=2195&height=1182
predicted_means <- apply(y.tilde.exp, 2, mean)

# Compute the actual mean price for each observation in the test data
actual_means <- exp(test_data$log_price)

# Compute a measure of predictive performance
RMSE <- sqrt(mean((predicted_means - actual_means)^2))


rsquared = 1 - (sum((actual_means - predicted_means)^2)/sum((actual_means - mean(actual_means))^2))

plot(actual_means,predicted_means)


test_data$y_tilde = predicted_means


names(test_data)
test_data %>% ggplot(aes(x=price,y=y_tilde,color = as.factor(lujo))) + geom_jitter(alpha=0.5,shape = 1) +
  geom_smooth(method = 'lm',se = FALSE) +
  facet_wrap(vars(distrito2)) +
  theme_bw()


print(RMSE)
# enhance the model
# add playa variable per districte and barri, are correlated with barris...

ggplot(test_data,aes(x = rooms2, y = price)) + 
  geom_boxplot() +
  geom_jitter(width=0.2)



t<- read_csv(here::here('Desktop','1_projects','TFM','1_data','2_data_Idealista','1_raw','extraction_2023-06-05','datos_scrapping_2023-06-05.csv'))



t_lm <- lm(log(price)~ square_mt + rooms,data = t)
summary(t_lm)

t %>% filter(rooms <= 5) %>%
  ggplot(aes(x = as.factor(rooms), y = price)) + 
  geom_boxplot() +
  geom_jitter(width=0.2)


view(t %>% filter(rooms >= 7))
view(t %>% filter(square_mt <  10))
view(t %>% filter(rooms == 0))

view(t %>% filter(price >= 15000))
