
# 4. Visualization --------------------------------------------------------

data_idealista <- readRDS(here::here('Desktop','1_projects','TFM','1_data','2_data_Idealista','data_modelling.RDS'))

colnames(data_idealista)



data_idealista %>% ggplot(aes(price)) + geom_histogram() + labs(title = "Distribution of price")
data_idealista %>% ggplot(aes(log_price)) + geom_histogram() + labs(title = "Distribution of log price")


N <- nrow(data_idealista)
barri <- as.numeric(data_idealista$barri)
barri_name <- unique(data_idealista$barri)

# Data visualization
price_barri <- data_idealista %>%
  group_by(barri) %>%
  summarise(log_price_mean = mean(log_price),
            log_price_sd = sd(log_price),
            log_meters = mean(log(square_mt)),
            n = length(barri)) %>%
  mutate(log_price_se = log_price_sd / sqrt(n)) # mean values per county


ggplot(data_idealista) +
  geom_boxplot(aes(y = log_price,
                   x = fct_reorder(barri, log_price, mean)),
               colour = "gray") +
  geom_point(aes(y = log_price,
                 x = fct_reorder(barri, log_price, mean)),
             colour = "gray") + 
  geom_point(data = price_barri,
             aes(x = fct_reorder(barri, log_price_mean),
                 y = log_price_mean),
             colour = "black") +
  coord_flip() +
  labs(y = "log(price)", x = "")




