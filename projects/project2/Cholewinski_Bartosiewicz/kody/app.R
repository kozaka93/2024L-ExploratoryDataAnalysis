library(shiny)
library(dplyr)
library(leaflet)
library(bslib)
library(ggplot2)
library(shinydashboard)
library(shinythemes)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(shinyWidgets)
library(shinycssloaders)

thematic::thematic_shiny()

data <- read.csv("Border_Crossing_Entry_Data_20240507.csv")
data$Year <- as.numeric(substr(data$Date, 5, 8))
unique(data$Measure)

choices <- c("Autobusy" = "Buses", 
             "Ciężarówki" = "Trucks", 
             "Pasażerowie autobusów" = "Bus Passengers", 
             "Puste ciężarówki" = "Truck Containers Empty", 
             "Pociągi" = "Trains", 
             "Piesi" = "Pedestrians", 
             "Pojazdy osobowe" = "Personal Vehicles", 
             "Pasażerowie pociągów" = "Train Passengers", 
             "Załadowane ciężarówki" = "Truck Containers Loaded", 
             "Załadowane wagony towarowe" = "Rail Containers Loaded", 
             "Puste wagony kolejowe" = "Rail Containers Empty", 
             "Pasażerowie pojazdów osobowych" = "Personal Vehicle Passengers")

choices_2 <- c("Nie"= "NO",
               "Tak"= "YES")

choices_3 <- c("Wszystkie"="Wszystkie","Wypadek śmiertelny" ="DEATH",
               "Bez rannych" = "NO_HARM",
               "Poszkodowani" = "INJURIES")

napisy <- c("Buses" = "autobusy", 
            "Trucks" = "ciężarówki", 
            "Bus Passengers" = "pasażerowie autobusów", 
            "Truck Containers Empty" = "puste ciężarówki", 
            "Trains" = "pociągi", 
            "Pedestrians" = "piesi", 
            "Personal Vehicles" = "pojazdy osobowe", 
            "Train Passengers" = "pasażerowie pociągów", 
            "Truck Containers Loaded" = "załadowane ciężarówki", 
            "Rail Containers Loaded" = "załadowane wagony towarowe", 
            "Rail Containers Empty" = "puste wagony kolejowe", 
            "Personal Vehicle Passengers" = "pasażerowie pojazdów osobowych")

dane <- read.csv("accidents_clean_2.csv")
data_to_chart <- read.csv("data_clean_2.csv")
data_to_wordcloud <- read.csv("data_clean_3.csv")

# Zastosowanie motywu
theme <- shinytheme("cosmo")

# Nagłówek
header <- dashboardHeader(title = "Transport w USA")

# Pasek boczny
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Rozkład liczby rannych osób", tabName = "rozklad", icon = icon("car-crash")),
    menuItem("Przekroczenia granic", tabName = "granica", icon = icon("map"))
  )
)

# Ciało aplikacji
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "rozklad",
            fluidRow(
              valueBox(
                value = paste0(
                  round(length(data_to_chart[data_to_chart$alc_involved == "YES", ]$CASENUM) / length(data_to_chart$CASENUM), 4) * 100, "%"),
                subtitle = "Wypadki z udziałem alkoholu",
                icon = icon("beer-mug-empty"),
                color = "green"
              ),
              valueBox(
                value = round(mean(dane$NUM_INJ, na.rm = TRUE), 2),
                subtitle = "Średnia liczba rannych w wypadkach",
                icon = icon("medkit"),
                color = "yellow"
              ),
              valueBox(
                value = paste0(
                  round(length(data_to_chart[dane$rodzaj_wyp == "DEATH", ]$CASENUM) / length(dane$REGION), 4) * 100, "%"),
                subtitle = "Wypadki śmiertelne",
                icon = icon("skull-crossbones"),
                color = "red"
              )
            ),
            fluidRow(
              column(width = 6,
                     box(
                       title = "Liczba rannych w zależności od godziny",
                       width = 12,
                       solidHeader = TRUE,
                       status = "primary",
                       plotOutput("plot4") %>% withSpinner(color = "#0dc5c1")
                     )
              ),
              column(width = 6,
                     box(
                       title = "Liczba rannych w zależności od pory dnia",
                       width = 12,
                       solidHeader = TRUE,
                       status = "primary",
                       plotOutput("inny_rozklad") %>% withSpinner(color = "#0dc5c1")
                     )
              )
            ),
            fluidRow(
              column(width = 12,
                     box(
                       title = "Ustaw swoje filtry",
                       width = 12,
                       solidHeader = TRUE,
                       status = "info",
                       selectInput("czy_alkohol1", "Czy w wypadku brał udział pijany kierowca?", choices = choices_2),
                       selectInput("injury_type1", "Rodzaj obrażeń", choices =  choices_3)
                     )
              )
            ),
            fluidRow(
              column(width = 6,
                     box(
                       title = "Chmura słów najczęstszych okoliczności wypadków",
                       width = 12,
                       solidHeader = TRUE,
                       status = "info",
                       plotOutput("wordcloud") %>% withSpinner(color = "#0dc5c1")
                     )
              ),
              column(width = 6,
                     box(
                       title = "Liczba rannych w zależności od regionu",
                       width = 12,
                       solidHeader = TRUE,
                       status = "info",
                       plotOutput("rozklad_osob") %>% withSpinner(color = "#0dc5c1")
                     )
              )
            )
    ),
    tabItem(tabName = "granica",
            fluidRow(
              valueBox(
                value = napisy[data %>% group_by(Measure) %>% summarise(n = n()) %>% top_n(1) %>% select(Measure) %>% pull()],
                subtitle = "Najczęstszy środek transportu",
                icon = icon("truck"),
                color = "purple"
              ),
              valueBox(
                value = data %>% group_by(State) %>% summarise(sum = sum(Value)) %>% top_n(1) %>% select(State) %>% pull(),
                subtitle = "Najwięcej przekroczeń (stan)",
                icon = icon("map"),
                color = "blue"
              ),
              valueBox(
                value = data %>% group_by(Year) %>% summarise(sum = sum(Value)) %>% top_n(1) %>% select(Year) %>% pull(),
                subtitle = "Najwięcej przekroczeń (rok)",
                icon = icon("calendar-alt"),
                color = "red"
              )
            ),
            fluidRow(
              column(width = 6,
                     box(
                       title = "Środek transportu",
                       width = 12,
                       solidHeader = TRUE,
                       status = "info",
                       selectInput("granica", "Wybrany środek transportu", choices = choices)
                     ),
                     box(
                       title = "Lata",
                       width = 12,
                       solidHeader = TRUE,
                       status = "info",
                       sliderInput("rok", "Wybrany zakres lat", min = min(data$Year), max = max(data$Year), value = c(min(data$Year), max(data$Year)))
                     )
              ),
              column(width = 6,
                     box(
                       title = "Mapa przedstawiająca przekroczenia granic",
                       width = 12,
                       solidHeader = TRUE,
                       status = "info",
                       textOutput("text"),
                       leafletOutput("distPlot") %>% withSpinner(color = "#0dc5c1")
                     )
              )
            ),
            fluidRow(
              box(
                title = "Przejścia przez granice w wybranych latach",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                plotOutput("plot1") %>% withSpinner(color = "#0dc5c1")
              )
            ),
            fluidRow(
              box(
                title = "Przekroczenia granic z podziałem na stany",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                plotOutput("plot2") %>% withSpinner(color = "#0dc5c1")
              )
            )
    )
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body,
  skin = "blue"
)

server <- function(input, output) {
  
  dane$TimeOfDay <- cut(dane$HOUR,
                        breaks = c(-Inf, 6, 12, 18, Inf),
                        labels = c("Noc", "Rano", "Popołudnie", "Wieczór"),
                        right = FALSE)
  
  output$rozklad_osob <- renderPlot({
    dane_filtr <- dane %>%
      filter(czy_byl_alk == input$czy_alkohol1)
    
    if (nrow(dane_filtr) == 0) {
      return(NULL)
    }
    
    ggplot(dane_filtr, aes(x = factor(reg), y = NUM_INJ, fill = reg)) +
      geom_violin(alpha = 0.8, size = 0.5) +
      labs(y = "Liczba rannych", x = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 10), axis.title.x = element_blank(), legend.position = "none")
  })
  
  output$distPlot <- renderLeaflet({
    filtered_data <- data %>%
      filter(Value != 0) %>%
      mutate(Year = as.numeric(substr(Date, 5, 8)),
             Radius = log(Value))
    
    filtered_data <- filtered_data %>%
      filter(Year >= input$rok[1] & Year <= input$rok[2], Measure == input$granica)
    
    if (nrow(filtered_data) == 0) {
      return(NULL)
    }
    
    colors <- colorRampPalette(c("yellow", "red"))(max(filtered_data$Radius) + 1)
    filtered_data$Color <- colors[filtered_data$Radius + 1]
    
    leaflet(filtered_data) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = ~Radius,
        color = ~Color,
        label = ~paste("Wartość:", Value),
        popup = ~paste("Rok:", Year, "<br>Środek transportu:", Measure)
      )
  })
  
  output$text <- renderText({
    paste("Przekroczenia granicy przez", napisy[input$granica], "w latach", input$rok[1],"-", input$rok[2])
  })
  
  output$plot4 <- renderPlot({
    data_to_chart_filtr <- data_to_chart %>%
      filter(alc_involved == input$czy_alkohol1)
    
    if (input$injury_type1 != "Wszystkie") {
      data_to_chart_filtr <- data_to_chart_filtr %>%
        filter(injury_type == input$injury_type1)
    }
    
    if (nrow(data_to_chart_filtr) == 0) {
      return(NULL)
    }
    
    ggplot(data_to_chart_filtr, aes(x = HOUR)) +
      geom_histogram(binwidth = 1, fill = "#0dc5c1", color = "white", alpha = 0.8) +
      labs(title = "Rozkład wypadków w zależności od godziny", x = "Godzina", y = "Liczba wypadków") +
      theme_minimal() +
      theme(axis.text = element_text(size = 10), plot.title = element_text(hjust = 0.5))
  })
  
  output$wordcloud <- renderPlot({
    words <- data_to_wordcloud$HARM_EVNAME
    wordcloud(words, min.freq = 10, colors = brewer.pal(9, "Dark2"))
  })
  
  output$plot1 <- renderPlot({
    data_plot1 <- data %>%
      filter(Year >= input$rok[1] & Year <= input$rok[2], Measure == input$granica)
    
    if (nrow(data_plot1) == 0) {
      return(NULL)
    }
    
    ggplot(data_plot1, aes(x = Year, y = Value)) +
      geom_bar(stat = "identity", fill = "#0dc5c1", alpha = 0.8) +
      labs(x = paste("Przekroczenie granicy przez", napisy[input$granica]), y = "Liczba przekroczeń") +
      theme_minimal() +
      theme(axis.text = element_text(size = 10), plot.title = element_text(hjust = 0.5))
  })
  
  output$plot2 <- renderPlot({
    data_plot2 <- data %>%
      filter(Year >= input$rok[1] & Year <= input$rok[2], Measure == input$granica) %>%
      group_by(State) %>%
      summarise(Value = sum(Value)) %>%
      arrange(desc(Value))
    
    if (nrow(data_plot2) == 0) {
      return(NULL)
    }
    
    ggplot(data_plot2, aes(x = reorder(State, -Value), y = Value)) +
      geom_bar(stat = "identity", fill = "#0dc5c1", alpha = 0.8) +
      labs(x = paste("Przekroczenia granicy przez", napisy[input$granica]), y = "Liczba przekroczeń") +
      theme_minimal() +
      theme(axis.text = element_text(size = 10), plot.title = element_text(hjust = 0.5))
  })
  
  output$inny_rozklad <- renderPlot({
    inne_dane <- dane %>%
      filter(czy_byl_alk == input$czy_alkohol1)
    
    if (nrow(inne_dane) == 0) {
      return(NULL)
    }
    
    ggplot(inne_dane, aes(x = TimeOfDay, y = NUM_INJ, fill = TimeOfDay)) +
      geom_boxplot(alpha = 0.8, size = 0.5) +
      labs(x = "Pora dnia", y = "Liczba rannych", fill = "Pora dnia") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 10), axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5), legend.position = "none")
  })
  
}

shinyApp(ui = ui, server = server)
