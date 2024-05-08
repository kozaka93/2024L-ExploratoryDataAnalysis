
library(plotly)
library(dplyr)


df <- data.frame(
  czas = seq(as.Date("2021-01-01"), as.Date("2024-01-01"), by = "month"),
  inflacja_CPI = c(0.1, 2.6, 2.4, 3.2, 4.4, 5.0, 5.5, 6.3, 6.8, 7.8, 8.6, 8.6, 9.2, 10.0, 10.9, 11.1, 12.4, 13.9, 14.8, 15.5, 16.1, 16.6, 17.5, 17.7, 17.2, 16.6, 15.9, 14.8, 14.4, 13.2, 11.0, 10.3, 9.6, 9.1, 8.2, 6.6, 5.5),
  stopa_referencyjna_NBP =c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1, 1.25, 1.75, 2.25, 3.5, 4.0, 4.5, 5.5, 5.75, 5.5, 6.0, 6.5, 6.5,6.75,6.75,6.75,6.75,6.75,6.75,6.75,6.75,6.75,6.75, 6.75, 6.5, 6.0, 5.75, 5.75,5.75),
  ICM = c(7.5,6,5.5,6.5,7.5,9,10.2,10,12,14.3,13.8,15.5,16.8,17,16.8,17.5,15.5,12.5,11.3,10.3,7.5,5.8,6,4.5,4.5,2.8,2.5,2,1.8,8.5,10.5,11.5,15.5,18,19,20.5,21)
)


ui <- fluidPage(
  titlePanel("Wskaźniki Ekonomiczne - Inflacja i Stopy Procentowe"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "cechy",
        "Wskaźniki do wyświetlenia:",
        choices = list(
          "Inflacja CPI" = "inflacja",
          "Stopa Referencyjna NBP" = "stopa",
          "ICM podkluczyk.pl" = "icm"
        ),
        selected = "inflacja"
      ),
      dateRangeInput(
        "zakres",
        "Zakres dat:",
        start = "2021-01-01",
        end = "2024-01-01",
        min = "2021-01-01",
        max = "2024-01-01",
        format ="mm/yyyy"
      )
    ),
    mainPanel(
      plotlyOutput("Displot")
    )
  )
)

server <- function(input, output) {
  output$Displot <- renderPlotly({
    filtered_data <- df %>%  filter(czas >= input$zakres[1] ) %>% 
      filter(czas <= input$zakres[2])
    
    p <- plot_ly(filtered_data, x = ~czas,hoverinfo = 'y')
    
    if ("inflacja" %in% input$cechy) {
      p <- add_lines(p, y = ~inflacja_CPI, name = "Inflacja CPI", line = list(color = "blue"))
    }
    if ("stopa" %in% input$cechy) {
      p <- add_lines(p, y = ~stopa_referencyjna_NBP, name = "Stopa Referencyjna NBP", line = list(color = "red"))
    }
    if ("icm" %in% input$cechy) {
      p <- add_lines(p, y = ~ICM, name = "ICM ", line = list(color = "orange"))
    }
    
    p <- p %>% layout(
      title = "Wskaźniki Ekonomiczne",
      xaxis = list(title = "Data"),
      yaxis = list(title = "Wartość [%]")
    )
    
    p
  })
}


shinyApp(ui, server)