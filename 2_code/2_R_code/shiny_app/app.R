library(tidyverse)
library(shiny)
library(sf)
library(leaflet)

# ui = fluidPage(
#   
#   titlePanel("Rent Map Barcelona"),
#   
#   sidebarLayout(
#     
#     sidebarPanel = sidebarPanel(sliderInput('years', 'Years', min = 1621, max = 2000, value = c(1700, 1750))),
#     mainPanel = mainPanel(
#       
#       leafletOutput(outputId = 'map')
#       
#       )
#   )
#   
# )
# 
# server = function(input, output){}
# 
# shinyApp(ui, server)
# 
# 
# demo(nc, ask = FALSE, echo = FALSE)
# plot(st_geometry(nc))

# # path_shp_barcelona = here::here('Desktop','1_projects','TFM',"1_data","3_data_Barris_Barcelona","0301040100_Barris_ADM_ETRS89.shp")
# path_shp_barcelona = here::here('Desktop','1_projects','TFM',"1_data","3_data_Barris_Barcelona","barris_clean.shp")
# 
# barcelona_shape <- sf::st_read(path_shp_barcelona)
# 
# barcelona_shape <- st_transform(barcelona_shape, "+proj=longlat +datum=WGS84")

# str(barcelona_shape)

data_date = "2023-05-03"

path_modelling = paste0("data_lm_cook_",data_date,".RDS")

barcelona_rent<- readRDS(here::here('Desktop','1_projects','TFM','1_data','2_data_Idealista',path_modelling))

path_shp_barcelona = here::here('Desktop','1_projects','TFM',"1_data","3_data_Barris_Barcelona","barris_clean.shp")
# path_shp_barcelona = here::here('Desktop','1_projects','TFM',"1_data","3_data_Barris_Barcelona","0301040100_Barris_ADM_ETRS89.shp")

barcelona_shape <- sf::st_read(path_shp_barcelona)

names(barcelona_rent)

barcelona_rent_gp = barcelona_shape %>% mutate(id_barri = as.numeric(BARRI)) %>% left_join(barcelona_rent %>% select(id_barri,price,square_mt,rooms) %>% 
                                                                                             group_by(id_barri) %>% summarise_all(mean), by = c("id_barri")) %>% ungroup()


barcelona_rent_gp <- st_transform(barcelona_rent_gp, "+proj=longlat +datum=WGS84")
# plot(st_geometry(barcelona_shape))
# 
# leaflet(st_geometry(barcelona_shape))

rent_palette <- colorNumeric(palette = "Blues", domain = barcelona_rent_gp$price)


ui <- fluidPage(
  fluidRow(
    column(width = 9,
           fluidRow(style = "height:400px"),leafletOutput("map"))
  )
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = barcelona_rent_gp,
        fillColor = ~rent_palette(price),
        fillOpacity = 0.5,
        color = "black",
        weight = 1,
        popup = ~paste("Barri: ", NOM,
                       "<br>Mean Square Meters: ", square_mt,
                       "<br>Rooms: ", rooms,
                       "<br>Mean Rent Price: $", price)
      ) %>%
      addLegend(
        pal = rent_palette,
        values = barcelona_rent_gp$price,
        title = "Mean Rent",
        position = "bottomright"
      )
      
  })

}




shinyApp(ui, server)

