library(tidyverse)
library(broom.mixed)
library(car)

theme_set(theme_minimal())

# 4. Visualization --------------------------------------------------------

data_idealista <- readRDS('1_data/2_data_Idealista/data_modelling_2023-05-03.RDS')

colnames(data_idealista)



data_idealista %>% ggplot(aes(price)) + geom_histogram() + labs(title = "Distribution of price")
data_idealista %>% ggplot(aes(log_price)) + geom_histogram() + labs(title = "Distribution of log price")


data_idealista %>% ggplot(aes(square_mt)) + geom_histogram() + labs(title = "Distribution of square meters")
data_idealista %>% ggplot(aes(log(square_mt))) + geom_histogram() + labs(title = "Distribution of log square meters")


N <- nrow(data_idealista)
barri <- as.numeric(data_idealista$id_barri)
barri_name <- unique(data_idealista$barri)

barri_name

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



# bayes models viz --------------------------------------------------------

# data cleaned using cook distance


# POOLED ------------------------------------------------------------------


data_date = "2023-05-03"

path_modelling = paste0("data_lm_cook_",data_date,".RDS")

data_cook<- readRDS(here::here('1_data','2_data_Idealista',path_modelling))

data_cook$barri <- as.factor(data_cook$barri)


N= nrow(data_cook)
barri_name <- unique(data_cook$barri)
barri <- as.integer((data_cook$barri))
J <- length(unique(barri))
y <- data_cook$log_price
x <- log(data_cook$square_mt)

id_barrio <- as.character(sample(barri_name,10))
# data pooled

fit_pooled <- readRDS(here::here('1_data','2_data_Idealista','3_fitted_data','model_pooled.RDS'))

price_summary_pooled <- tidy(fit_pooled, conf.int = T, level = 0.8, rhat = T, ess = T)

df_pooled  <- tibble(
  barri = barri_name,
  model = "pooled",
  intercept = price_summary_pooled$estimate[1],
  slope = price_summary_pooled$estimate[2]
)


# id_barrio<- c("la Guineueta",
#                "la Vall d'Hebron", "Canyelles", "la Trinitat Nova", "el Raval", "la Dreta de l'Eixample", "el Barri Gòtic",
#                "Sants")


df_model <- df_pooled %>%
  left_join(data_cook, by = "barri") %>%
  filter(barri %in% id_barrio)

ggplot(df_model) +
  geom_point(aes(log(square_mt), log_price)) +
  geom_abline(aes(intercept = intercept, slope = slope, color = model)) +
  facet_wrap(~ barri, ncol = 4) +
  theme(legend.position = "bottom")

# data no pooled

fit_no_pooled <- readRDS(here::here('1_data','2_data_Idealista','3_fitted_data','model_no_pooled.RDS'))

price_summary_no_pooled <- tidy(fit_no_pooled, conf.int = T, level = 0.8, rhat = T, ess = T)

df_no_pooled  <- tibble(
  barri = barri_name,
  model = "no_pooled",
  intercept = price_summary_no_pooled$estimate[1:J],
  slope = price_summary_no_pooled$estimate[J+1]
)

# id_barrio<- c("la Guineueta",
#                "la Vall d'Hebron", "Canyelles", "la Trinitat Nova", "el Raval", "la Dreta de l'Eixample", "el Barri Gòtic",
#                "Sants")

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

# data hierarchical

fit_hier <- readRDS(here::here('1_data','2_data_Idealista','3_fitted_data','model_fitted_hier.RDS'))

price_summary_hier <- tidy(fit_hier, conf.int = T, level = 0.8, rhat = T, ess = T)

df_multilevel  <- tibble(
  barri = barri_name,
  model = "multilevel",
  intercept = price_summary_hier$estimate[1:J],
  slope = price_summary_hier$estimate[J+1]
)

df_model <- bind_rows(df_pooled, df_no_pooled, df_multilevel) %>%
# df_model <- bind_rows(df_multilevel) %>%
  left_join(data_cook, by = "barri") %>%
  filter(barri %in% id_barrio)

ggplot(df_model) +
  geom_jitter(aes(log(square_mt), log_price)) +
  geom_abline(aes(intercept = intercept, slope = slope, color = model)) +
  facet_wrap(~ barri, ncol = 4) +
  scale_x_continuous(breaks = 0:1) +
  theme(legend.position = "bottom")


# lm coeff ----------------------------------------------------------------

model_path = paste0("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/model_cook_2023-05-03.RDS")

model_cook = readRDS(model_path)


vif(model_cook)

# Calcular el VIF
vif_values <- vif(model_cook)

# Convertirlo en un marco de datos
vif_df <- data.frame(vif_values)

vif_df$variables <- row.names(vif_df)

colnames(vif_df) <- c("VIF","df","VIF_ajustado","variables")

vif_df = vif_df %>% select(variables,VIF,df,VIF_ajustado)


# plot model --------------------------------------------------------------

coefs <- tidy(model_cook, conf.int = TRUE)

# Identifica las variables que corresponden a los barrios
coefs$term_group <- ifelse(grepl("^barri", coefs$term), "barri", "other")

# Organiza los términos primero por el grupo (barri u other) y luego alfabéticamente
coefs <- coefs %>%
  arrange(term_group, term)

coefs = coefs %>% filter(!grepl("^barri|(Intercept)",coefs$term))

# Elimina la columna term_group ya que ya no la necesitamoshttp://127.0.0.1:41615/graphics/plot_zoom_png?width=1920&height=1027
coefs$term_group <- NULL

# Convierte la variable term en una variable categórica con el orden específico
coefs$term <- factor(coefs$term, levels = coefs$term)

# Crea el gráfico
ggplot(coefs, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Variables", y = "Estimación", title = "Coeficientes e Intervalos de Confianza del 95%")


# summary(model_cook)

coefs

library(xtable)
print(xtable(coefs), type = "latex")


library(knitr)

variables_description <- data.frame(
  Variable = c("log_price", "barri", "square_mt", "asc", "rooms2", "new_planta", "flag_planta", "wc2", "terraza", "exterior", "amueblado", "lujo"),
  Description = c("Logarithm of Price", "Neighborhood", "Square Meters", "Elevator", "Number of Rooms", "New Floor", "Flag Floor", "Number of Bathrooms", "Terrace", "Exterior", "Furnished", "Luxury"),
  Type = c("Dependent", rep("Independent", 11)),
  DataType = c("Numeric", "Categorical", "Numeric", "Binary", "Numeric", "Binary", "Binary", "Numeric", "Binary", "Binary", "Binary", "Binary")
)

# latex_table <- kable(variables_description, format = "latex", booktabs = TRUE, caption = "Description of the Variables in the Model")

xtable(variables_description, type = 'latex')

print(latex_table)
