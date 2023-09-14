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

# vif_df$variables <- row.names(vif_df)

colnames(vif_df) <- c("VIF","df","VIF_ajustado")

# vif_df = vif_df %>% select(variables,VIF,df,VIF_ajustado)

latex_table <- xtable(vif_df, caption = "Factor de la inflación de la varianza")

# Imprimir la tabla en LaTeX
print(latex_table, type = "latex")

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



# Model comparison viz ----------------------------------------------------

linear_model = readRDS(paste0("./1_data/2_data_Idealista/model_cook_tidy.RDS"))
# 
# pooled <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_pooled_tidy.RDS") # pooled 1325.239, 0.5236905
# no_pooled <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_no_pooled.RDS") # no pooled 1435.512
# hier_1 <- readRDS("C:/Users/ggari/Desktop/1_projects/TFM/1_data/2_data_Idealista/3_fitted_data/model_hierarchical_2.RDS") # 1385.68, 0.479253

path = paste0("./1_data/2_data_Idealista/3_fitted_data/model_pooled.RDS")

pooled = readRDS(path)

path = paste0("./1_data/2_data_Idealista/3_fitted_data/model_no_pooled.RDS")

no_pooled = readRDS(path)

path = paste0("./1_data/2_data_Idealista/3_fitted_data/model_hierarchical_2.RDS")

hier_1 <- readRDS(path)

path = paste0("./1_data/2_data_Idealista/3_fitted_data/model_4_9.RDS")

hier_cov = readRDS(path)

linear_model

summary(pooled)$summary

tidy_pooled = tidyMCMC(pooled,rhat = T,ess=T)
tidy_no_pooled = tidyMCMC(no_pooled,rhat = T,ess=T)
tidy_hier_1 = tidyMCMC(hier_1,rhat = T,ess=T)
tidy_hier_cov = tidyMCMC(hier_cov,rhat = T,ess=T)


linear_model = linear_model %>% filter(!grepl("^barri|(Intercept)",linear_model$term))

tidy_pooled = tidy_pooled %>% filter(!grepl("^b0|sigma_y|lp_",tidy_pooled$term)) %>% mutate(term = ifelse(term == "log_mt","log_smt",term))

tidy_no_pooled = tidy_no_pooled  %>% filter(!grepl("^b0|sigma_y|lp_",tidy_no_pooled$term))

tidy_hier_1 = tidy_hier_1 %>% filter(!grepl("^b0|sigma_y|lp_|mu_a|sigma_a",tidy_hier_1$term))

tidy_hier_cov = tidy_hier_cov %>% filter(!grepl("^b0|sigma_y|lp_|mu_a|sigma_a|g_0|g_1",tidy_hier_cov$term))

linear_model$model = "lm"
tidy_pooled$model = "pooled"
tidy_no_pooled$model = "no_pooled"
tidy_hier_1$model = "hierarchical"
tidy_hier_cov$model = "hierarchical_cov"

viz_lm = linear_model %>% select(term,estimate,std.error,model)
viz_pool = tidy_pooled %>% select(term,estimate,std.error,model)
viz_no_pool = tidy_no_pooled %>% select(term,estimate,std.error,model)
viz_hier = tidy_hier_1 %>% select(term,estimate,std.error,model)
viz_hier_cov = tidy_hier_cov %>% select(term,estimate,std.error,model)

remove(viz_1)

# viz_1 = rbind(viz_lm,viz_pool,viz_no_pool,viz_hier,viz_hier_cov)
viz_1 = rbind(viz_no_pool,viz_hier,viz_hier_cov)


ggplot(viz_1, aes(x = term, y = estimate, group = model, color = model)) +
  geom_point() +
  # geom_line() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  ggtitle("Estimate with Standard Error of Different Models") +
  xlab("Term") +
  ylab("Estimate") +  coord_flip()

# barris plots

linear_model_barri = linear_model %>% filter(grepl("^barri|(Intercept)",linear_model$term))

tidy_pooled_barri = tidy_pooled %>% filter(grepl("^b0",tidy_pooled$term)) %>% mutate(term = ifelse(term == "log_mt","log_smt",term))

tidy_no_pooled_barri = tidy_no_pooled  %>% filter(grepl("^b0",tidy_no_pooled$term))

tidy_hier_1_barri = tidy_hier_1 %>% filter(grepl("^b0",tidy_hier_1$term))

tidy_hier_cov_barri = tidy_hier_cov %>% filter(grepl("^b0",tidy_hier_cov$term))


mapping_terms = read_rds("./1_data/2_data_Idealista/mapping_barri_coeff.rds")


tidy_no_pooled_barri = tidy_no_pooled_barri %>% left_join(mapping_terms, by = 'term')
tidy_hier_1_barri = tidy_hier_1_barri %>% left_join(mapping_terms, by = 'term')
tidy_hier_cov_barri = tidy_hier_cov_barri %>% left_join(mapping_terms, by = 'term')


tidy_no_pooled_barri$model = "no_pooled"
tidy_hier_1_barri$model = "hierarchical"
tidy_hier_cov_barri$model = "hierarchical_cov"

viz_2 = rbind(tidy_no_pooled_barri,tidy_hier_1_barri,tidy_hier_cov_barri)

ggplot(viz_2 %>% filter(plot == '1'), aes(x = barri_name, y = estimate, group = model, color = model)) +
  geom_point() +
  # geom_line() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  # ggtitle("Coefficientes de los barrios con mayor/ menor oferta") +
  xlab("barrio") +
  ylab("Coeficiente ") +  coord_flip()

