library(shiny)
library(leaflet)
library(dplyr)
# Load the data
# data(barcelona_rent)

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
# Load the data
# data(barcelona_rent)

# Define the UI
ui <- fluidPage(
  titlePanel("Barcelona Rent Prices"),
  leafletOutput("map")
)

# Define the server logic
server <- function(input, output) {
  
  # Render the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = barcelona_rent_gp,
        fillColor = ~colorPalette(price),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~paste0("<b>Neighborhood: </b>", neighborhood,
                        "<br><b>Mean Rent Price: </b>", price, "€")
      ) %>%
      addLegend(
        position = "bottomright",
        pal = colorPalette,
        values = ~price,
        title = "Mean Rent Price (€)",
        opacity = 1
      )
  })
  
  # Define the color palette for the map polygons
  colorPalette <- reactive({
    colorNumeric(
      palette = "Blues",
      domain = barcelona_rent_gp$price
    )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)




# # Define the UI
# 
# ui <- fluidPage(
#   titlePanel("Barcelona Rent Prices"),
#   leafletOutput("map")
# )
# 
# # Define the server logic
# server <- function(input, output) {
#   
#   # Render the map
#   output$geometry <- renderLeaflet({
#     leaflet() %>%
#       addTiles() %>%
#       addPolygons(
#         data = barcelona_rent_gp,
#         fillColor = ~colorPalette(price),
#         fillOpacity = 0.7,
#         color = "white",
#         weight = 1,
#         popup = ~paste0("<b>Neighborhood: </b>", NOM,
#                         "<br><b>Mean Rent Price: </b>", price, "€")
#       ) %>%
#       addLegend(
#         position = "bottomright",
#         pal = colorPalette,
#         values = ~price,
#         title = "Mean Rent Price (€)",
#         opacity = 1
#       )
#   })
#   
#   # Define the color palette for the map polygons
#   colorPalette <- reactive({
#     colorNumeric(
#       palette = "Blues",
#       domain = barcelona_rent$price
#     )
#   })
# }
# 
# # Run the Shiny app
# shinyApp(ui = ui, server = server)
# 
# 
# leaflet(barcelona_rent_gp) %>%
#   addTiles() %>%
#   addPolygons(
#     data = barcelona_rent_gp,
#     # fillColor = ~colorPalette(price),
#     fillOpacity = 0.7,
#     color = "white",
#     weight = 1,
#     popup = ~paste0("<b>Neighborhood: </b>", NOM,
#                     "<br><b>Mean Rent Price: </b>", price, "€")
#   )


# library(shiny)
# library(leaflet)
# library(rgdal) # Required for reading shapefiles

# Define the UI
# ui <- fluidPage(
#   titlePanel("Display Shapefile with Leaflet"),
#   leafletOutput("map")
# )
# 
# # Define the server logic
# server <- function(input, output) {
#   
#   # Read the shapefile
#   shapefile <- sf::st_read(path_shp_barcelona)
#   
#   # Convert to GeoJSON
#   geojson <- geojsonio::geojson_json(shapefile, geometry = "wkb")
#   
#   # Render the map
#   output$map <- renderLeaflet({
#     leaflet() %>%
#       addTiles() %>%
#       addGeoJSON(data = geojson)
#   })
# }
# 
# # Run the Shiny app
# shinyApp(ui = ui, server = server)
