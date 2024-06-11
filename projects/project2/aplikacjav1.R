# Wstęp ----
## Packages (pakiety) ----
require(shiny)
require(shinythemes)
require(tidyverse)
require(data.table)
require(extrafont)
require(leaflet)
require(geojsonio)
require(geojson)
require(dplyr)
require(sp)
require(sf)
require(leaflet.extras)
require(plotly)
require(DT)

## Zmienne global (globalne) ----
nazwy_stanow = fread("states.csv")
nowy_jork <- c(40.7128, -74.0060)
reasons = c(
  "Cell Phone (hand-Held)",
  "Alcohol Involvement",
  "Drugs (illegal)",
  "Eating or Drinking",
  "Tinted Windows",
  "Texting",
  "Cell Phone (hands-free)",
  "Traffic Control Disregarded",
  "Aggressive Driving/Road Rage",
  "Fell Asleep",
  "Using On Board Navigation Device",
  "Other Electronic Device",
  "Listening/Using Headphones"
)
granice_nowy_jork_geojson <-
  geojsonio::geojson_read("Borough-Boundaries.geojson", what = "sp")

# UI ----
ui = fluidPage(theme = shinytheme("darkly"), 
               titlePanel(h1("Transport analysis in New York", 
                             align = "center")), 
               mainPanel(
  tabsetPanel(
    type = "tabs",
    ## Panel 1 ----
    tabPanel("Car accidents", sidebarLayout(
      sidebarPanel(
        selectInput(
          "year1",
          "Choose year:",
          choices = 2012:2022,
          selected = 2017
        ),
        sliderInput(
          "dateRange",
          "Choose date range:",
          min = as.Date("2017-01-01"),
          max = as.Date("2017-12-31"),
          value = c(as.Date("2017-01-01"), as.Date("2017-12-31")),
          timeFormat = "%Y-%m-%d"
        ),
        radioButtons(
          inputId = "czy_zabici",
          label = "Choose options:",
          choices = c("Injured", "Killed")
        ),
        submitButton("Refresh", icon("refresh"))
      ),
      mainPanel(
        h2("Accidents"),
        leafletOutput("wykres1", height = "600px", width = "175%")
      )
    ), ),
    ## Panel 2 ----
    tabPanel(
      "Causes of accidents",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "year2",
            "Choose year:",
            choices = 2012:2022,
            selected = 2017
          ),
          submitButton("Refresh", icon("refresh"))
        ),
        mainPanel(
          h2("Causes of accidents"),
          plotlyOutput("wykres2", height = "600px", width = "175%")
        )
      )
    ),
    ## Panel 3 ----
    tabPanel("Map of accidents", sidebarLayout(
      sidebarPanel(
        selectInput(
          "year3",
          "Choose year:",
          choices = 2012:2022,
          selected = 2017
        ),
        selectizeInput(
          inputId = "reason",
          label = "Choose the reason:",
          choices = reasons,
          selected = "Fell Asleep",
          options = list(onInitialize = I('function() { this.setValue(""); }'))
        ),
        h3("Summary"),
        tableOutput("tabela"),
        submitButton("Refresh", icon("refresh"))
      ),
      mainPanel(
        h2("Map"),
        leafletOutput("wykres3", height = "600px", width = "175%")
        )
    )),
    ## Panel 4 ----
    tabPanel(
      "Englishmen in New York",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "year4",
            "Choose year:",
            choices = 2014:2017,
            selected = 2016
          ),
          selectizeInput(
            inputId = "state",
            label = "Choose the state:",
            choices = nazwy_stanow[,1],
            selected = "Alabama",
            options = list(onInitialize = I('function() { this.setValue(""); }'))
          ),
          submitButton("Refresh", icon("refresh"))
        ),
        mainPanel(
          column(12,
          h2("Outsiders' parking tickets"),
          plotlyOutput("wykres4a", height = "600px", width = "175%"),
          h2("Make of cars receiving tickets"),
          plotlyOutput("wykres4b", height = "600px", width = "175%")
          ))
        )
      )
    )
  )
)
# SERWER ----
server = function(input, output, session) {
  observeEvent(input$year1, {
    year <- input$year1
    updateSliderInput(
      session,
      "dateRange",
      min = as.Date(paste0(year, "-01-01")),
      max = as.Date(paste0(year, "-12-31")),
      value = c(as.Date(paste0(year, "-01-01")), as.Date(paste0(year, "-12-31")))
    )
  })
  
  ## Panel 1 ----
  output$wykres1 = renderLeaflet({
    collisions <- fread(paste("wypadki", input$year1, ".csv", sep = ""))
    collisions <- collisions %>%
      filter(BOROUGH != "" &
               !is.na(LATITUDE) & !is.na(LONGITUDE)) %>%
      filter(good_date %in% input$dateRange)
    
    if (input$czy_zabici == "Killed") {
      collisions = collisions %>%
        filter(NUMBER.OF.PERSONS.KILLED != 0)
    }
    else if (input$czy_zabici == "Injured") {
      collisions = collisions %>%
        filter(NUMBER.OF.PERSONS.INJURED != 0 &
                 NUMBER.OF.PERSONS.KILLED == 0)
    }
    
    mapa_nowy_jork <- leaflet() %>%
      setView(lng = nowy_jork[2],
              lat = nowy_jork[1],
              zoom = 10) %>%
      addTiles() %>%
      addPolygons(
        data = granice_nowy_jork_geojson,
        color = "blue",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.2
        
      ) %>%
      addHeatmap(
        lng = ~ LONGITUDE,
        lat = ~ LATITUDE,
        data = collisions,
        blur = 20,
        max = 0.05,
        radius = 15
      )
    
    mapa_nowy_jork
  })
  ## Panel 2 ----
  output$wykres2 = renderPlotly({
    wypadki = fread(paste("wypadki", input$year2, ".csv", sep = ""))
    przyczyna <- wypadki %>%
      select(good_date2,
             BOROUGH,
             CONTRIBUTING.FACTOR.VEHICLE.1,
             NUMBER.OF.PERSONS.INJURED) %>%
      filter(BOROUGH != "") %>%
      filter(CONTRIBUTING.FACTOR.VEHICLE.1 != "" &
          CONTRIBUTING.FACTOR.VEHICLE.1 != "Unspecified") %>%
      group_by(BOROUGH, CONTRIBUTING.FACTOR.VEHICLE.1) %>%
      summarise(number_of_accidents = n()) %>%
      arrange(desc(number_of_accidents)) %>%
      top_n(10)
    
    przyczyna$CONTRIBUTING.FACTOR.VEHICLE.1 <-
      factor(przyczyna$CONTRIBUTING.FACTOR.VEHICLE.1)
    
    wykresik = plot_ly(
      przyczyna,
      x = ~ BOROUGH,
      y = ~ number_of_accidents,
      color = ~ CONTRIBUTING.FACTOR.VEHICLE.1,
      type = 'bar') %>%
      layout(
        xaxis = list(title = "Cause of accident"),
        yaxis = list(title = "Number of accidents"),
        barmode = 'stack'
      )
    wykresik
    
    
  })
  ## Panel 3 ----
  output$wykres3 = renderLeaflet({
    wypadki <- fread(paste("wypadki", input$year3, ".csv", sep = "")) %>%
      filter(BOROUGH != "") %>% 
      filter(CONTRIBUTING.FACTOR.VEHICLE.1 == input$reason)
    
    mapawyp <- leaflet() %>%
      setView(lng = nowy_jork[2],
              lat = nowy_jork[1],
              zoom = 10) %>%
      addTiles() %>%
      addMarkers(
        data = wypadki,
        popup = ~ paste("Street:", ON.STREET.NAME, "<br>", "Zipcode:", ZIP.CODE)
      ) %>%
      addPolygons(
        data = granice_nowy_jork_geojson,
        color = c("blue", "red", "yellow", "green", "purple"),
        weight = 1,
        opacity = 1,
        fillOpacity = 0.2
      )
    mapawyp
  })
  output$tabela = renderTable({
    wypadki <- fread(paste("wypadki", input$year3, ".csv", sep = "")) %>%
      filter(CONTRIBUTING.FACTOR.VEHICLE.1 != "") %>%
      filter(BOROUGH != "") %>%
      filter(ON.STREET.NAME != "") %>%
      filter(CONTRIBUTING.FACTOR.VEHICLE.1 == input$reason) %>%
      group_by(BOROUGH) %>%
      summarize(ACCIDENTS  = n()) %>% 
      bind_rows(summarise(., across(where(is.numeric), sum),
                          across(where(is.character), ~'TOTAL')))
    
    
  })
  
  ## Panel 4 ----
  output$wykres4a = renderPlotly({
    tikety = fread(paste("tikety", input$year4, ".csv", sep = "")) %>%
      left_join(nazwy_stanow, by = c("Registration.State" = "Abbreviation")) %>% 
      filter(Registration.State != "NY" & is.na(State) == FALSE) %>%
      group_by(State) %>%
      summarise(n = n())
    wykresik = plot_ly(
      tikety,
      x = ~ reorder(State, -n),
      y = ~ n,
      type = 'bar'
    ) %>%
      layout(
        xaxis = list(title = "Registration state"),
        yaxis = list(title = "Number of tickets", type = "log")
      )
    wykresik
  })
  
  output$wykres4b = renderPlotly({
    tikety2 = fread(paste("tikety", input$year4, ".csv", sep = "")) %>%
      left_join(nazwy_stanow, by = c("Registration.State" = "Abbreviation")) %>%
      filter(State == input$state) %>%
      group_by(Vehicle.Make) %>%
      summarise(n = n()) %>% 
      top_n(10)
    
    wykresik2 = plot_ly(
      tikety2,
      x = ~ reorder(Vehicle.Make, -n),
      y = ~ n,
      type = 'bar') %>%
      layout(
        xaxis = list(title = "Vehicle make"),
        yaxis = list(title = "Number of cars")
        )
    wykresik2
    
  })
  
}

shinyApp(ui = ui, server = server)