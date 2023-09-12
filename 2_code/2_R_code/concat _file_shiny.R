library(here)


here("1_data","2_data_idealista")


list_files = list.files(here("1_data","2_data_idealista"))

filter_files = list_files[grepl("data_lm_cook_2023-",list_files)]


df_concat = data.frame()

for(f in filter_files){
  
  print(f)
  
  tmp_df = readRDS(here("1_data","2_data_idealista",f))
  
  df_concat = rbind(df_concat,tmp_df)
}

# nrow(df_concat)

# dup_ids= df_concat %>% group_by(id) %>% count() %>% filter(n>1) %>% pull(id)

# x = df_concat %>% filter(id %in% dup_ids)

# df_concat %>% filter( id == '553641')

df_concat = df_concat %>% arrange(across(c(id,price))) # arrange para asegurar
# quitamos el mas caro

df_concat = df_concat %>% distinct(id,.keep_all = T)


path = paste0(here("1_data","2_data_idealista","concat_shiny.RDS") )

saveRDS(df_concat,path)



