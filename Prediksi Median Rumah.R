library(shiny)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(bg = "#f0f8ff", fg = "#222222", primary = "#ff6347"),
  
  # CSS untuk background gambar rumah dan panel semi transparan
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('https://share.google/images/eSrenqfkmLLktzAn7'); /* link gambar rumah */
        background-size: cover;
        background-attachment: fixed;
      }
      .well, .panel {
        background-color: rgba(255, 255, 255, 0.8);
        padding: 20px;
        border-radius: 10px;
      }
    "))
  ),
  
  titlePanel("Prediksi Median House Value", windowTitle = "House Predictor"),
  
  sidebarLayout(
    sidebarPanel(class = "well",
                 selectInput("ocean_proximity", "Ocean Proximity:",
                             choices = c("<1H OCEAN", "INLAND", "ISLAND", "NEAR BAY", "NEAR OCEAN")),
                 numericInput("longitude", "Longitude:", value = -122.23),
                 numericInput("latitude", "Latitude:", value = 37.88),
                 numericInput("housing_median_age", "Housing Median Age:", value = 28),
                 numericInput("total_rooms", "Total Rooms:", value = 880),
                 numericInput("total_bedrooms", "Total Bedrooms:", value = 129),
                 numericInput("population", "Population:", value = 322),
                 numericInput("households", "Households:", value = 126),
                 numericInput("median_income", "Median Income:", value = 8.3252),
                 actionButton("predict", "Hitung Prediksi", class = "btn-primary")
    ),
    
    mainPanel(class = "well",
              h3("Hasil Prediksi:"),
              verbatimTextOutput("prediction")
    )
  )
)

server <- function(input, output) {
  
  prediksi <- eventReactive(input$predict, {
    # Dummy variabel 4 kolom, <1H OCEAN = baseline
    ocean_INLAND <- ifelse(input$ocean_proximity == "INLAND", 1, 0)
    ocean_ISLAND <- ifelse(input$ocean_proximity == "ISLAND", 1, 0)
    ocean_NEARBAY <- ifelse(input$ocean_proximity == "NEAR BAY", 1, 0)
    ocean_NEAROCEAN <- ifelse(input$ocean_proximity == "NEAR OCEAN", 1, 0)
    
    # Hitung prediksi
    y_pred <- 175930.13 +
      (-94014.98 * ocean_INLAND) +
      (103571.64 * ocean_ISLAND) +
      (2192.51 * ocean_NEARBAY) +
      (2088.45 * ocean_NEAROCEAN) +
      (12.87 * input$longitude) +
      (-1684.77 * input$latitude) +
      (1022.39 * input$housing_median_age) +
      (27.82 * input$total_rooms) +
      (-42.81 * input$total_bedrooms) +
      (-76.45 * input$population) +
      (144.63 * input$households) +
      (2.25 * input$median_income)
    
    return(round(y_pred, 2))
  })
  
  output$prediction <- renderText({
    prediksi()
  })
}

shinyApp(ui = ui, server = server)
