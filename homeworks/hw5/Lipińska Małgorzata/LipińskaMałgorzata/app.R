

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

ui <- fluidPage(
  # Tytuł
  titlePanel("Share of energy products in total energy available, 2020 (in %)"),
  
  # przycisk zmieniający źródło energii
  sidebarLayout(
    sidebarPanel(
      radioButtons("option", "Sources:",
                   choices = c("Total petroleum products", "Natural gas", "Renewable energy", "Nuclear energy", "Solid fossil fuels"),
                   selected = NULL)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)


server <- function(input, output) {

  
  dane <- reactive({
    
  dane <- read_excel("C:/Users/gosia/Documents/eurostat.xlsx", sheet = "Sheet 1")
  dane%>%
    filter(TIME=='2020')->dane
  dane_long <- pivot_longer(dane, cols = -c(TIME, SIEC), names_to = "Państwo", values_to = "Wartość")
  dane_long%>%
    group_by(Państwo, SIEC)->dane_long
  
  total_per_country <- dane_long %>%
    filter(SIEC == "Total")
  
  dane_long%>%
    filter(SIEC!='Total')->dane_long
  
  dane_merged <- dane_long %>%
    left_join(total_per_country, by = "Państwo")%>%
    mutate(Wartość=Wartość.x/Wartość.y*100)
  
  return(dane_merged)
  })
  
 
  
  observe({
  filtered_data<-dane()%>%
    filter(SIEC.x==input$option)
  
  
  output$distPlot <- renderPlot({
    ggplot(filtered_data,aes(x=Państwo, y=Wartość))+
      geom_bar(stat = "identity",fill='hotpink') +
      geom_text(aes(label = round(Wartość, 2)), vjust = -0.5, size = 2) +
      facet_wrap(~SIEC.x) +
      labs(x = "Państwo", y = "Wartość")+
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
