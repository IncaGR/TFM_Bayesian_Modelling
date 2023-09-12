
# Obtain the map ID <-> Nom Barri in Barcelona shp

mapIdBarri <- function(df){
  "Obtain mapping from Barcelona map shapefile ID barri, Nom Barri"
  
  colnames(df) <- tolower(colnames(df))
  
  temp = df %>% dplyr::select(barri,nom) %>% arrange(barri) %>% as.data.frame()
  
  temp = temp %>% dplyr::select(barri,nom) %>% arrange(barri,nom) %>%
    rename(id_barri = barri, barri = nom)
  

  return(temp)

}

regex_nom_barris <- function(df,col_name){
  "
  Add a new column in lower case 'col_name'.
  Add a second column only with the important string of the neighbour
  
  "
  df = df %>%
    mutate(
      barri_lower = tolower(col_name),
      regex_barris = barri_lower,
      regex_barris = gsub(pattern = "^el |^la |^l'|^les |d'", replacement = "", x = regex_barris),
      regex_barris = gsub(pattern = " de la | - | de l'| i la | i el | i les | i els | d'en | del | de "
                          , " ", regex_barris),
      regex_barris = gsub(pattern = " el | la | i | d'| dels |, "
                          , " ", regex_barris),
      regex_barris = gsub(pattern = " el |-"
                          , " ", regex_barris),
      regex_barris = gsub(pattern = "barrio "
                          , "", regex_barris),
      regex_barris = gsub(pattern = "barri "
                          , "", regex_barris),
      regex_barris = gsub(pattern = "Barrio "
                          , "", regex_barris),
      regex_barris = gsub(pattern = " dhebron"
                          , " hebron", regex_barris),
      regex_barris = gsub(pattern = " leixample"
                          , " eixample", regex_barris),
      regex_barris = gsub(pattern = "lantiga "
                          , "antiga ", regex_barris),
      regex_barris = gsub(pattern = " den "
                          , " ", regex_barris),
      regex_barris = gsub(pattern = " larpa "
                          , " arpa ", regex_barris),
      regex_barris = gsub(pattern = " en "
                          , " ", regex_barris),
      regex_barris = gsub(pattern = "l'"
                          , "", regex_barris),
      regex_barris = gsub(pattern = "les "
                          , "", regex_barris)
    ) %>%
    select(-c(barri_lower))
  
  return(df)
}




check_names_open_data <- function(df,df_open_data) {
  
  "
  Check the consistency of the neigbourhood names in the Idealista dataset and the
  Open Data Barcelona sets.
  
  Return the names that are not included in the short list of names
  and returns NULL if all the names are included.
  "
  
  len_idealista = length(unique(df$key_open))
  len_open = length(unique(df_open_data$regex_barris))
  
  
  barris_idealista = unique(df$key_open)
  
  barris_open = unique(df_open_data$regex_barris)
  
  if (len_idealista < len_open){
    
    idx = barris_idealista %in% barris_open
    
    result = barris_idealista[!idx]
    
  }
  
  if (len_idealista > len_open) {
    
    idx = barris_open %in% barris_idealista
    
    result = barris_open[!idx]
  }
  
  # results = all(result)
  
  return(result)
}


read_API_data <- function(){
  
  "
  Returns the csv of Open Data Barcelona storaged and check the consistency on names
  with the function check_names_open_data().
  
  "
  # wd = "C:/Users/ggari/Desktop/1_projects/TFM"
  # path_open_data <- "Desktop/1_projects/TFM/1_data/1_data_API/dataset_opendata"
  
  for (f in files){
    path_open_data <- "1_data/1_data_API/dataset_opendata"
    print(f)
    f_split = strsplit(f,".csv")
    path_open_data =  here(path_open_data,f)
    print(path_open_data)
    print(f_split)
    string_ = toString(f_split)
    df_ = assign(string_, read.csv(path_open_data,encoding = "UTF-8"))
    
    colnames(df_) <- tolower(colnames(df_))
    
    
    
    try(assign(string_,df_,envir = parent.frame()))
    
    # if ("addresses_neighborhood_name" %in% colnames(df_)){
    #   df_ = regex_nom_barris(df_,df_$addresses_neighborhood_name)
    #   
    #   print(check_names_open_data(df = data_idealista,df_open_data = df_))
    #   
    #   try(assign(string_,df_,envir = parent.frame()))
    # }
    # 
    # if ("nom_barri" %in% colnames(df_)){
    #   df_ = regex_nom_barris(df_,df_$nom_barri)
    #   
    #   print(check_names_open_data(df = data_idealista,df_open_data = df_))
    #   
    #   try(assign(string_,df_,envir = parent.frame()))
    # }
    # 
  }
  
}



groupingDataOpenData <-function(df_idealista,df_open_data, col_name =NA){
  
  df_idealista$id_barri = as.integer(df_idealista$id_barri)
  df_idealista$codi_districte = as.integer(df_idealista$codi_districte)
  
  if("codi_barri" %in% colnames(df_open_data)) {
    print("codi_barri")
    
    # name_col = cat("n_",name,"_barri")
    
    df_open_data$nom_districte = tolower(df_open_data$nom_districte)
    
    groupData = df_open_data %>%
      group_by(codi_barri) %>%
      count(name = paste0("n_",col_name,"_barri",sep = ""), sort = T)
    
    print(groupData)
    
    groupDataDis = df_open_data %>%
      group_by(codi_districte) %>%
      count(name = paste0("n_",col_name,"_districte",sep = ""), sort = T)
    
    df_idealista_final = left_join(df_idealista,groupData,by = c("id_barri"="codi_barri"))
    df_idealista_final = left_join(df_idealista_final,groupDataDis,by = c("codi_districte"="codi_districte"))
    
    
    return(df_idealista_final)
    
  }
  
  if("addresses_neighborhood_id" %in% colnames(df_open_data)){
    
    print("addresses_neighborhood_id")
    
    df_open_data$addresses_district_name = tolower(df_open_data$addresses_district_name)
    
    groupData = df_open_data %>%
      group_by(addresses_neighborhood_id) %>%
      count(name = paste0("n_",col_name,"_barri",sep = ""), sort = T)
    
    print(groupData)
    
    groupDataDis = df_open_data %>%
      group_by(addresses_district_id) %>%
      count(name = paste0("n_",col_name,"_districte",sep = ""), sort = T)
    
    
    df_idealista_final = left_join(df_idealista,groupData,by = c("id_barri"="addresses_neighborhood_id"))
    df_idealista_final = left_join(df_idealista_final,groupDataDis,by = c("codi_districte"="addresses_district_id"))
    
    
    return(df_idealista_final)
    
    
  }
}


