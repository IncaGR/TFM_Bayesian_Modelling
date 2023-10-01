library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)
library(here)

library(rsconnect)

file_path = "concat_shiny.RDS"

barcelona_rent<- readRDS(here::here('1_data','2_data_Idealista',file_path))

# barcelona_rent<- readRDS(here::here(file_path))

# names(barcelona_rent)

barcelona_rent = barcelona_rent %>% filter(lujo == 0)

path_shp_barcelona = here::here("1_data","3_data_Barris_Barcelona","barris_clean.shp")
# path_shp_barcelona = here::here("barris_clean.shp")

barcelona_shape <- sf::st_read(path_shp_barcelona)

# names(barcelona_rent)

barcelona_rent_gp = barcelona_shape %>% mutate(id_barri = as.numeric(BARRI)) %>% left_join(barcelona_rent %>% select(id_barri,price,square_mt,rooms) %>%
                                group_by(id_barri) %>% summarise_all(mean), by = c("id_barri")) %>% ungroup()





barcelona_rent_gp <- st_transform(barcelona_rent_gp, "+proj=longlat +datum=WGS84")


barcelona_rent_gp$price =  round(barcelona_rent_gp$price,1)

# barcelona_rent_gp = as.data.frame(barcelona_rent_gp)
# 
# 
# barcelona_rent_gp <- sf::st_as_sf(barcelona_rent_gp)

fit <- readRDS(here::here("1_data","2_data_Idealista","3_fitted_data","model_4_9.RDS")) # modelo
# fit <- readRDS(here::here("model_4_9.RDS")) # modelo

mpaping = here("1_data","2_data_Idealista","mapping_barri_coeff.rds")
# mpaping = here("mapping_barri_coeff.rds")

mapping = readRDS(mpaping)

# names(mapping)
# 
mapping$num = 1:65




# 5 -----------------------------------------------------------------------


# Define the UI
ui <- fluidPage(
  titlePanel("Precios Alquiler Barcelona"),
  
  navbarPage(
    "Barcelona Rent Prediction",
    tabPanel("Map", 
             leafletOutput("map")),
    tabPanel("Select Features",
             selectInput("neighborhood", "Barrio:",
                         choices = unique(barcelona_rent_gp$NOM)),
             selectInput("bedrooms", "Número de habitaciones:",
                         choices = c("0", "1", "2", "3", "4 o más"),
                         selectize = FALSE),
             selectInput("bathrooms", "Número de baños:",
                         choices = c("1", "2", "3", "4 o más"),
                         selectize = FALSE),
             checkboxInput("furnished", "Amueblado", value = FALSE),
             checkboxInput("terrace", "Terraza", value = FALSE),
             checkboxInput("lift", "Ascensor", value = FALSE),
             checkboxInput("luxe", "Lujo", value = FALSE),
             textInput("sqmeters", "Metros Cuadrados:", value = "80"),
             actionButton("predictButton", "Generar Predicción"),
             
             # Agregar la pestaña "Results" dentro de "Select Features"
             conditionalPanel(
               condition = "input.predictButton > 0",
               tabsetPanel(
                 tabPanel("Results",
                          tableOutput("predictionTable")
                 )
               )
             ),
             
             # Agregar una pestaña para el histograma y el control deslizante de bins
             plotOutput("priceHistogram")
    )
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
        fillColor = ~colorPalette()(price),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~paste0("<b>Barrio: </b>", NOM,
                        "<br><b>Precio medio: </b>", price, "€")
      ) %>%
      addLegend(
        position = "bottomright",
        pal = colorPalette(),
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
  
  # Crear una variable reactiva para almacenar las predicciones lineales
  linear_predictions <- reactive({
    # Obtener los valores seleccionados por el usuario
    selected_neighborhood <- input$neighborhood
    selected_bedrooms <- input$bedrooms
    selected_bathrooms <- input$bathrooms
    selected_furnished <- input$furnished
    selected_terrace <- input$terrace
    selected_lift <- input$lift
    selected_luxe <- input$luxe
    selected_sqmeters <- as.numeric(input$sqmeters)  # Asegurarse de que sea numérico
    
    # Asumiendo que tienes el modelo previamente ajustado en `fit`
    sims <- rstan::extract(fit)
    
    barrio_index = filter(mapping, barri_name == selected_neighborhood)$num
    
    # Definir los valores de las características para la predicción
    neighborhood_value <- selected_neighborhood
    rooms2_1 <- ifelse(selected_bedrooms == '1',1,0)
    rooms2_2 <-ifelse(selected_bedrooms == '2',1,0)
    rooms2_3 <- ifelse(selected_bedrooms == '3',1,0)  # Puedes ajustar estos valores según las selecciones del usuario
    rooms2_4 <- ifelse(selected_bedrooms == '4 o más',1,0)
    wc2_2 <- ifelse(selected_bathrooms == '2',1,0)
    wc2_3 <- ifelse(selected_bathrooms == '3',1,0)
    wc2_4 <- ifelse(selected_bathrooms == '4 o más',1,0)
    terraza <- ifelse(selected_terrace, 1, 0)
    asc <- ifelse(selected_lift, 1, 0)
    smt <- log(selected_sqmeters)
    lujo <- ifelse(selected_luxe, 1, 0)
    amueblado <- ifelse(selected_furnished, 1, 0)
    
    # Inicializar un vector para almacenar las predicciones lineales
    n.sims <- nrow(sims$b0)
    linear_predictions <- numeric(n.sims)
    
    # Calcular las predicciones lineales para cada muestra
    for (i in 1:n.sims) {
      linear_predictions[i] <- sims$b0[i, barrio_index] + # 1 neighborhood_value
        rooms2_1 * sims$rooms2_1[i] +
        rooms2_2 * sims$rooms2_2[i] +
        rooms2_3 * sims$rooms2_3[i] + 
        rooms2_4 * sims$rooms2_4[i] +
        wc2_2 * sims$wc2_2[i] +
        wc2_3* sims$wc2_3[i] +
        wc2_4* sims$wc2_4[i] +
        terraza * sims$terraza[i] +
        asc * sims$asc[i] +
        smt * sims$log_smt[i] +
        lujo * sims$lujo[i] +
        amueblado * sims$amueblado[i]
    }
    
    # Calcular las predicciones de precios usando el modelo
    predicted_prices <- exp(linear_predictions)
    
    return(predicted_prices)
  })
  
  # Crear una función para renderizar el histograma
  output$priceHistogram <- renderPlot({
    # Obtener el número de bins seleccionados por el usuario
    bins <- 30
    
    # Crear un dataframe con las predicciones
    prices_df <- data.frame(Precio_Predicho = linear_predictions())
    
    # Crear el histograma usando ggplot2
    ggplot(prices_df, aes(x = Precio_Predicho)) +
      geom_histogram(binwidth = (max(prices_df$Precio_Predicho) - min(prices_df$Precio_Predicho)) / bins
                     , fill = "#00B8E7") +
      labs(title = "Distribución de Precios Predichos",
           x = "Precio Predicho") +
      theme_minimal()
  })
  
  # Calcular las predicciones de precios, crear la tabla de resultados y mostrarla
  observe({
    # Obtener las predicciones lineales
    preds <- linear_predictions()
    
    # Calcular las predicciones de precios usando el modelo
    predicted_prices <- (preds)
    mean_predicted_price <- mean(predicted_prices)
    
    # Obtener los valores seleccionados por el usuario
    selected_neighborhood <- input$neighborhood
    selected_bedrooms <- input$bedrooms
    selected_bathrooms <- input$bathrooms
    selected_furnished <- input$furnished
    selected_terrace <- input$terrace
    selected_lift <- input$lift
    selected_luxe <- input$luxe
    selected_sqmeters <- as.numeric(input$sqmeters)
    
    # Crear una tabla de resultados
    results_df <- data.frame(
      Barrio = selected_neighborhood,
      Metros_cuadrados = selected_sqmeters,
      Habitaciones = selected_bedrooms,
      Baños = selected_bathrooms,
      Amueblado = ifelse(selected_furnished, "Sí", "No"),
      Terraza = ifelse(selected_terrace, "Sí", "No"),
      Ascensor = ifelse(selected_lift, "Sí", "No"),
      Lujo = ifelse(selected_luxe, "Sí", "No"),
      Precio_Predicho = mean_predicted_price
    )
    
    # Mostrar la tabla de resultados
    output$predictionTable <- renderTable({
      results_df
    })
  })
  
  
  
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

# runApp(appDir = "C:/Users/galag/Desktop/1_projects/TFM/2_code/2_R_code/shiny_app_2")