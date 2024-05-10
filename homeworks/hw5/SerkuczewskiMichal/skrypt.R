library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

procenty <- data.frame(
  plec = rep(c("Kobiety", "Mężczyźni"), each = 6),
  zdanie = rep(c(
    "Otwarte granice między państwami członkowskimi",
    "Polska otrzymuje fundusze unijne",
    "Wzrost bezpieczeństwa Polski",
    "Możliwość kształcenia poza Polską",
    "Inne",
    "Nie wiem/Trudno powiedzieć"
  ), 2),
  procent = c(
    30.0,
    25.0,
    20.0,
    15.0,
    5.0,
    5.0,
    34.0,
    18.0,
    29.0,
    7.0,
    6.0,
    6.0
  )
)

ui <- fluidPage(
  titlePanel("Zalety obecności Polski w UE - Analiza według płci"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plec", "Wybierz płeć:", 
                  choices = c("Obie", "Kobiety", "Mężczyźni"),
                  selected = "Obie")
    ),
    mainPanel(
      plotlyOutput("pieChart")
    )
  )
)

server <- function(input, output) {
  output$pieChart <- renderPlotly({
    
    if (input$plec == "Obie") {
      data <- procenty
    } else {
      data <- subset(procenty, plec == input$plec)
    }
    
    zlaczone <- aggregate(procent ~ zdanie, data, sum)
    
    plot_ly(
      zlaczone,
      labels = ~zdanie,
      values = ~procent,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "label+percent",
      marker = list(
        colors = c("darkviolet", "magenta", "darkgrey", "orange", "darkgreen", "blue"),
        line = list(color = 'white', width = 1)
      )
    ) %>%
      layout(
        title = paste("Zalety obecności Polski w UE -", input$plec)
      )
  })
}

shinyApp(ui = ui, server = server)