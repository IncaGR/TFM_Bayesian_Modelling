
# Modelling tunning -------------------------------------------------------

# We are goign to use two methods for tunning the linear model and thus find 
# outliers in the data. 1. Cook's distance, 2º dfbetas.

# Dfbetas:

# we have some variables that we know the coefficient sign.
# e.g) It is logical that the square meters variable has positive impact in the 
# price. 

coefficients_model <-c("square_mt","asc","amueblado","aire", 
                      "estudio","rooms","wc2")



coefficients(lm4)["rooms"]



if(coefficients(lm4)["rooms"] <0){
  stop("wrong direction coefficient")
}

for(c in coefficients_model){
  
  print(coefficients(lm4)[c]) 
}


# see is there are some deleteion during lm calculation
nrow(data_idealista) == nrow(dfbetas(lm4))

data_idealista$id_betas = 1:nrow(data_idealista)

betas = as.data.frame(dfbetas(lm4))

betas$id_betas = 1:nrow(betas)

# betas$rooms

# colnames(betas)

betas %>% select(id_betas, rooms) %>% arrange(rooms)

# ?slice_sample

tmp <- betas %>% select(id_betas, rooms) %>% slice_max(
          rooms,
          # prop = 0.995,
          n= -1)


view(data_idealista %>% filter(!id_betas %in% tmp$id_betas))

# view(data_idealista %>% filter(!id_betas %in% tmp$id_betas))


new_df = data_idealista %>% filter(id_betas %in% tmp$id_betas)


mod <- lm(reformulate("square_mt + asc + rooms","log_price"),
          new_df)

summary(mod)

summary(lm4)


mod2 <- lm(reformulate(regressors,"log_price"),
           new_df)


summary(mod2)
summary(lm2)
# # dfbetas calculation
# tmp = dfbetas(lm3)
# 
# df = data_betas 
# 
# colnames(tmp) <- gsub(pattern = "[()]|[ ]","",colnames(tmp))
# 
# colnames(tmp) <-  paste0(colnames(tmp),'_beta')
# 
# df_merge <- cbind(df,tmp)
# 
# 
# df_merge %>%filter(rooms_beta == min(rooms_beta)) 
# 
# 
# df_merge = df_merge %>%filter(!rooms_beta == min(rooms_beta)) 
# 
# mod <- lm(df_merge)

calc_dfbetas <- function(model){
  
  tmp = dfbetas(model)
  
  return(as.data.frame(tmp)) 
  
}





format_col_betas <- function(betas){
  
  colnames(betas) <- gsub(pattern = "[()]|[ ]","",colnames(betas))
  
  colnames(betas) <-  paste0(colnames(betas),'_beta')
  
  return(as.data.frame(betas))
  
}


observation_wrong_sign <- function(df){
  
  print(df %>%filter(rooms_beta == min(rooms_beta)))
  
}

remove_wrong_obs <- function(df){
  
  df = df %>%filter(!rooms_beta == min(rooms_beta)) 
  
  return(df)
}


# 1º betas
df = data_cook

betas <- dfbetas(lm3)


df_beta = format_col_betas(betas) %>% cbind(df)

observation_wrong_sign(df_beta)

df_beta = df_beta %>% remove_wrong_obs()

mod = lm(reformulate("square_mt + asc + amueblado + aire + 
                      estudio + rooms + wc2 + n_arbres_viaris_barri + cuaps
                      ","log_price"),
         df_beta)

summary(mod)


df_beta <- df_beta %>% select(- contains("_beta"))



df = calc_dfbetas(mod) %>% format_col_betas() %>% cbind(df_beta)


observation_wrong_sign(df)


df = df %>% remove_wrong_obs()


mod = lm(reformulate("square_mt + asc + amueblado + aire + 
                      estudio + rooms + wc2 + n_arbres_viaris_barri + cuaps
                      ","log_price"),
         df)

summary(mod)

df <- df %>% select(- contains("_beta"))



df2 = calc_dfbetas(mod) %>% format_col_betas() %>% cbind(df)


observation_wrong_sign(df2)


df2 = df2 %>% remove_wrong_obs()


mod = lm(reformulate("square_mt + asc + amueblado + aire + 
                      estudio + rooms + wc2 + n_arbres_viaris_barri + cuaps
                      ","log_price"),
         df2)

summary(mod)


df2 %>% filter(rooms_beta < 0) %>% arrange(desc(rooms_beta))
