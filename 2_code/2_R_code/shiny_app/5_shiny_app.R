library(shiny)
library(leaflet)
library(dplyr)
library(sf)
# Load the data
# data(barcelona_rent)

file_path = "concat_shiny.RDS"

barcelona_rent<- readRDS(here::here('1_data','2_data_Idealista',file_path))

names(barcelona_rent)

barcelona_rent = barcelona_rent %>% filter(lujo == 0)

path_shp_barcelona = here::here("1_data","3_data_Barris_Barcelona","barris_clean.shp")
# path_shp_barcelona = here::here('Desktop','1_projects','TFM',"1_data","3_data_Barris_Barcelona","0301040100_Barris_ADM_ETRS89.shp")

barcelona_shape <- sf::st_read(path_shp_barcelona)

names(barcelona_rent)

barcelona_rent_gp = barcelona_shape %>% mutate(id_barri = as.numeric(BARRI)) %>% left_join(barcelona_rent %>% select(id_barri,price,square_mt,rooms) %>% 
                                group_by(id_barri) %>% summarise_all(mean), by = c("id_barri")) %>% ungroup()


barcelona_rent_gp <- st_transform(barcelona_rent_gp, "+proj=longlat +datum=WGS84")
# Load the data
# data(barcelona_rent)

names(barcelona_rent_gp)

head(barcelona_rent_gp)

barcelona_rent_gp$price =  round(barcelona_rent_gp$price,1)

# Define the UI
ui <- fluidPage(
  titlePanel("Precios Alquiler Barcelona"),
  
  mainPanel(
    leafletOutput("map")
  )
  
)

# Define the server logic
server <- function(input, output) {
  
  # Render the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = barcelona_rent_gp,
        fillColor = ~colorPalette()(price), # Cambio aquí para usar colorPalette() correctamente
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~paste0("<b>Barrio: </b>", NOM,
                        "<br><b>Precio medio: </b>", price, "€")
      ) %>%
      addLegend(
        position = "bottomright",
        pal = colorPalette(), # Cambio aquí para usar colorPalette() correctamente
        values = barcelona_rent_gp$price,
        title = "Precio medio alquiler (€)",
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

