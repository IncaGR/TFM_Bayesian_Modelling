


# bar de copas
colnames(data_idealista)

regressors<-c(
  # "barri",
  "distrito2",
  "terraza",
  # "balcon",
  "estado" ,
  # "armarios",
  # "cocina",
  "amueblado",                  
  # "planta",
  # "calef",
  "asc",
  "aire",
  "exterior",                                    
  # "casa",
  "estudio",                    
  "wc2",
  "rooms2",
  # # "terraza_balcon",
  # "n_hospitals_barri",
  # # "n_hospitals_districte",
  # "hospitals",
  # "caps",
  "cuaps",
  "n_terrazas_barri",
  # # "n_terrazas_districte",
  # # "mean_cadires_b",
  "mean_superficie_b",
  "mean_taules_b",
  # "n_arbres_bcn_barri",
  # # "n_arbres_bcn_districte",
  "n_arbres_viaris_barri",
  # # "n_arbres_viaris_districte",
  "n_bar_copas_barri",
  # # "n_bar_copas_districte",
  "square_mt"
)

# data_idealista[is.na(data_idealista$n_c_comercials),]$n_c_comercials <- 0

# data_idealista = data_idealista %>%
#   replace_na(list(n_c_comercials = 0,
#                   Museus = 0))


lm1 <- lm(reformulate(regressors,"log_price"),
          data_idealista)

lm0 <- lm(reformulate("square_mt","log_price"),
          data_idealista)

summary(lm0)

lm0 <- lm(reformulate("square_mt + rooms2","log_price"),
          data_idealista)
summary(lm0)

# con variables de open data sale max R2 .675
# sin variables open data pero con la variable barrios en vez de distrito sale 0.69


summary(lm1)
vif(lm1)

plot(lm1,ask=F)

data_idealista[which(hatvalues(lm1)>0.99),]

# sort(cooks.distance(lm1))
cooksd =  cooks.distance(lm1)
data_idealista$cookd = cooks.distance(lm1)


data_idealista[data_idealista$cookd>0.01,]

plot(cooks.distance(lm1))
abline(h = 4*mean(cooksd, na.rm=T), col="red") 

# outlierTest(lm1)
# cook_test

# cook_test = data_idealista[data_idealista$cookd >(4*mean(cooksd, na.rm=T)),]
cook_test = data_idealista[data_idealista$cookd < 0.01,]


lm1 <- lm(reformulate(regressors,"log_price"),
          cook_test)

# con variables de open data sale max R2 .675
# sin variables open data pero con la variable barrios en vez de distrito sale 0.69


summary(lm1)
vif(lm1)

plot(lm1,ask=F)


# Modeling Bayesian ------------------------------------------------------


