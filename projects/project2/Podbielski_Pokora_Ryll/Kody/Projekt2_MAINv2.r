library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(shinycssloaders)
library(bslib)
library(maps)
library(mapdata)
library(leaflet)
library(leaflet.extras)
library(geojsonio)
library(htmltools)
library(plotly)

# Import data
pirate_attacks <- read.csv("pirate_attacks.csv")
country_codes <- read.csv("country_codes.csv")
country_indicators <- read.csv("country_indicators.csv")

pirate_attacks$attack_type <- as.factor(pirate_attacks$attack_type)
pirate_attacks$year <- as.numeric(substr(pirate_attacks$date, 1, 4))

# Merge with country codes by eez_country
pirate_attacks <- merge(pirate_attacks, country_codes, by.x = "eez_country", by.y = "country")

# Data for the scatter plot
ind <- country_indicators %>%
  left_join(country_codes) %>%
  filter(year == 2016) %>%
  rename(GDP_per_capita = GDP, total_fisheries = total_fisheries_per_ton) %>%
  select(country_name, region, corruption_index, unemployment_rate, GDP_per_capita, homicide_rate, population, total_fisheries)
  # group_by(country_name) %>%
  # summarise(region = first(region),
  #           corruption_index = mean(corruption_index, na.rm = T),
  #           unemployment_rate = mean(unemployment_rate, na.rm = T),
  #           GDP = mean(GDP, na.rm = T),
  #           homicide_rate = mean(homicide_rate, na.rm = T)) %>%


server <- function(input, output, session) {
  
  output$bar_years <- renderPlotly({
    data1 <- pirate_attacks %>%
      filter(year >= input$slider1[1] & year <= input$slider1[2])
    # Plot count of attacks in each year
    data1 %>%
      ggplot(aes(x = year)) +
      geom_bar(fill = "darkred", alpha = 0.8) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank()) +
      labs(
        x = "Year",
        y = "Number of attacks",
        title = "Number of pirate attacks in each year"
      )
  })
  
  output$Map <- renderLeaflet({
    
    # Filtering data based on the years selected by the user
    data1 <- pirate_attacks %>%
      filter(year >= input$slider1[1] & year <= input$slider1[2])
    
    # Labels for the point map
    labels_point <- paste("Date: ", data1$date, "<br>",
                          "Country: ", data1$country_name, "<br>",
                          "Date: ", data1$date, "<br>",
                          "Time: ", data1$time, "<br>",
                          "Attack type: ", data1$attack_type, "<br>",
                          "Vessel type: ", data1$vessel_type, "<br>"
    ) %>%
      lapply(htmltools::HTML)
    
    ### POINTMAP - Map with points representing pirate attacks
    PointMap <- leaflet(options = leafletOptions(minZoom=1.5)) %>%
      addTiles() %>%
      addCircles(data = data1,
                 lng = ~longitude,
                 lat = ~latitude,
                 radius = 1,
                 color = "darkred",
                 highlightOptions = highlightOptions(
                                    weight = 10,
                                    color = "darkred",
                                    fillOpacity = 1,
                                    bringToFront = TRUE
                                    ),
                 label = labels_point,
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px"),
                   textsize = "15px",
                   direction = "auto"
                 )
                 ) %>%
      setMaxBounds(-180, -90, 180, 90) %>%
      setView(lng = 0, lat = 15, zoom = 2)
    
    
      ### HEATMAP - Map with heatmap of pirate attacks
      HeatMap <- leaflet(options = leafletOptions(minZoom=1.5)) %>%
        addTiles() %>%
        addHeatmap(data = data1, lng = ~longitude, lat = ~latitude, blur = 15, radius = 7, gradient = "Reds") %>%
        setMaxBounds(-180, -90, 180, 90) %>%
        setView(lng = 0, lat = 15, zoom = 2)
      
      
      ### CARTOGRAM
      # Cartogram of how many times a country is listed as the one on which waters a pirate attack took place
      
      # Preparing attacks data
      attack_counts <- table(data1$eez_country)
      attack_counts <- as.data.frame(attack_counts)
      names(attack_counts) <- c("country", "attack_count")
      attack_counts <- merge(attack_counts, country_codes, by.x = "country", by.y = "country")
      # Merging with world map data
      world_geojson <- geojson_read("world.geo.json", what = "sp")
      cartogram_data <- world_geojson
      cartogram_data@data <- left_join(cartogram_data@data, attack_counts, by = c("iso_a3_eh" = "country"))
      # converting NA to zeros
      cartogram_data@data$attack_count[is.na(cartogram_data@data$attack_count)] <- 0
      # Creating a color palette
      pal <- colorBin("Reds", domain = cartogram_data$attack_count, bins = c(0, 2, 10, 50, 100, 200, 500, 1000, Inf))
      # Creating labels for the map
      labels_cartogram = sprintf(paste0("Country: ", cartogram_data@data$name, "<br>",
                                        "Number of attacks: ", cartogram_data@data$attack_count)) %>%
        lapply(htmltools::HTML)
      
      # Creating the map
      Cartogram <- leaflet(cartogram_data, options = leafletOptions(minZoom=1.5)) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal(attack_count),
          color = "white",
          weight = 1,
          opacity = 1,
          dashArray = "",
          fillOpacity = 0.8,
          highlight = highlightOptions(
            weight = 2,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          label = labels_cartogram
        ) %>%
        addLegend(pal = pal, values = ~attack_count,
                  title = "Number of attacks",
                  opacity = 0.8,
                  position = "bottomleft") %>%
        setMaxBounds(-180, -90, 180, 90) %>% # now set starting position
        setView(lng = 0, lat = 15, zoom = 2)
    
      if (input$map_type == "Point map") {
        return(PointMap)
      }
      else if(input$map_type == "Heat map") {
        return(HeatMap)
      }
      else {
        return(Cartogram)
      }
  })
  
  output$text_map <- renderText({
    "The map shows where pirate attacks happened from 1993 to 2020. Each point on the point map marks the exact spot of an attack. The heat map uses colors to show how many attacks happened in different areas, with darker colors meaning more attacks. The cartogram uses color shades to show how many attacks occurred in each country's waters, with more intense colors indicating more attacks. This visualization helps to see where and how often pirate activities took place during these years."
  })
  
  output$Attack_types_barplot <- renderPlotly({
    
    # Barplot of attack types for countries
    pirate_attacks %>%
       filter(year >= input$slider1[1] & year <= input$slider1[2]) %>%
       filter(country_name == input$country) %>%
       select(attack_type) %>%
       table() %>%
       data.frame() %>%
       ggplot(aes(x = attack_type, y = Freq)) +
       geom_col(fill = "darkred", alpha = 0.8) +
       theme_minimal() +
       theme(panel.grid.major.x = element_blank()) +
       labs(
         x = "Attack type",
         y = "Number of attacks",
       )
    
  })
  
  output$Attack_types_barplot_reg <- renderPlotly({
    
    # Barplot of attack types for regions
    pirate_attacks %>%
       filter(year >= input$slider1[1] & year <= input$slider1[2]) %>%
       filter(region == input$region) %>%
       select(attack_type) %>%
       table() %>%
       data.frame() %>%
       ggplot(aes(x = attack_type, y = Freq)) +
       geom_col(fill = "darkred", alpha = 0.8) +
       theme_minimal() +
       theme(panel.grid.major.x = element_blank()) +
       labs(
         x = "Attack type",
         y = "Number of attacks",
       )
    
  })
  
  output$text1 <- renderText({
    paste("The total number of attacks on this country's waters between ", input$slider1[1], " and ", input$slider1[2] ," is ", sum((pirate_attacks %>% filter(year >= input$slider1[1] & year <= input$slider1[2]))$country_name == input$country))
  })
  
  output$text2 <- renderText({
    paste("The total number of attacks on waters between ", input$slider1[1], " and ", input$slider1[2] ," in this region is ", sum((pirate_attacks %>% filter(year >= input$slider1[1] & year <= input$slider1[2]))$region == input$region))
  })
  
  output$Scatter <- renderPlotly({

    c1 <- case_when(input$cat1=="corruption_index" ~ "Corruption index",
                    input$cat1=="unemployment_rate" ~ "Unemployment rate",
                    input$cat1=="GDP_per_capita" ~ "GDP per capita",
                    input$cat1=="homicide_rate" ~ "Homicide rate",
                    input$cat1=="attack_count" ~ "Number of attacks",
                    input$cat1=="population" ~ "Population",
                    input$cat1=="total_fisheries" ~ "Total fisheries production (tons)")
    c2 <- case_when(input$cat2=="corruption_index" ~ "Corruption index",
                    input$cat2=="unemployment_rate" ~ "Unemployment rate",
                    input$cat2=="GDP_per_capita" ~ "GDP per capita",
                    input$cat2=="homicide_rate" ~ "Homicide rate",
                    input$cat2=="attack_count" ~ "Number of attacks",
                    input$cat2=="population" ~ "Population",
                    input$cat2=="total_fisheries" ~ "Total fisheries production (tons)")
    
    i <- ind %>%
      left_join(pirate_attacks %>%
                filter(year >= input$slider1[1] & year <= input$slider1[2]) %>%
                group_by(country_name) %>%
                summarise(attack_count = n())) %>%
      filter(!is.na(attack_count)) %>%
      rename(country = country_name)
    
    i <- i %>%
      select(country, region, input$cat1, input$cat2)
    if(input$cat1 == input$cat2){
      i[4] <- i[3]
    }
    colnames(i) <- c("country", "region", "x", "y")
    
    plot_ly(
      data = i,
      x =~x,
      y =~y,
      color=~region,
      type='scatter',
      mode='markers',
      text=~country,
      alpha=0.9
    ) %>%
      layout(
        xaxis = list(title = c1, range = c(0, max(i$x, na.rm = T)*1.05)),
        yaxis = list(title = c2, range = c(0, max(i$y, na.rm = T)*1.05))
      )
  })
  
  output$text3 <- renderText({
    # Corelation coefficient between two indicators on the scatter plot
    
    i <- ind %>%
      left_join(pirate_attacks %>%
                  filter(year >= input$slider1[1] & year <= input$slider1[2]) %>%
                  group_by(country_name) %>%
                  summarise(attack_count = n())) %>%
      filter(!is.na(attack_count))
    
    d1 <- case_when(input$cat1=="corruption_index" ~ "Corruption index",
                    input$cat1=="unemployment_rate" ~ "Unemployment rate",
                    input$cat1=="GDP_per_capita" ~ "GDP per capita",
                    input$cat1=="homicide_rate" ~ "Homicide rate",
                    input$cat1=="attack_count" ~ "Number of attacks",
                    input$cat1=="population" ~ "Population",
                    input$cat1=="total_fisheries" ~ "Total fisheries production (tons)")
    d2 <- case_when(input$cat2=="corruption_index" ~ "Corruption index",
                    input$cat2=="unemployment_rate" ~ "Unemployment rate",
                    input$cat2=="GDP_per_capita" ~ "GDP per capita",
                    input$cat2=="homicide_rate" ~ "Homicide rate",
                    input$cat2=="attack_count" ~ "Number of attacks",
                    input$cat2=="population" ~ "Population",
                    input$cat2=="total_fisheries" ~ "Total fisheries production (tons)")
    
    paste("The correlation coefficient between ", tolower(d1), " and ", tolower(d2), " is ", round(cor(i[[input$cat1]], i[[input$cat2]], use = "complete.obs"), 2))
  })
  
}

ui1 <- fluidPage(titlePanel("Map"),
                       fluidRow(
                         column(12,
                           radioButtons(
                             inputId = "map_type",
                             label = "Choose a map type:",
                             choices = c("Point map", "Heat map", "Cartogram")
                           )
                         )
                       ),
                       fluidRow(
                         column(12,
                           withSpinner(leafletOutput("Map"), type = 6, hide.ui = F, color = "darkred"), #AAD3DF
                           br(),
                           textOutput("text_map")
                         )
                       )
                 )

ui2 <- fluidPage(titlePanel("Types of attacks"),
                  fluidRow(
                    column(3,
                      br(),
                      selectInput(
                      inputId = "country",
                      label = "Choose a country:",
                      choices = unique(pirate_attacks$country_name),
                      selected = "Somalia"
                    ),
                    textOutput("text1")
                    ),
                    column(7,
                      withSpinner(plotlyOutput("Attack_types_barplot"), type = 6, hide.ui = F, color = "darkred")
                    )
                  ),
                  fluidRow(
                    column(3,
                      selectInput(
                      inputId = "region",
                      label = "Choose a region:",
                      choices = unique(pirate_attacks$region),
                      selected = "East Asia & Pacific"
                    ),
                    textOutput("text2"),
                    ),
                    column(7,
                      withSpinner(plotlyOutput("Attack_types_barplot_reg"), type = 6, hide.ui = F, color = "darkred")
                    )
                   )
                  )

ui3 <- fluidPage(titlePanel("Correlations"),
                 fluidRow(
                   column(3,
                          br(),
                          selectInput(
                            inputId = "cat1",
                            label = "Choose a category for the x-axis:",
                            choices = c(
                              "Number of attacks" = "attack_count",
                              "Corruption index" = "corruption_index",
                              "Unemployment rate" = "unemployment_rate",
                              "GDP per capita" = "GDP_per_capita",
                              "Homicide rate" = "homicide_rate",
                              "Population" = "population",
                              "Total fisheries" = "total_fisheries"
                            ),                            
                            selected = "attack_count"
                          ),
                          selectInput(
                            inputId = "cat2",
                            label = "Choose a category for the y-axis:",
                            choices = c(
                              "Number of attacks" = "attack_count",
                              "Corruption index" = "corruption_index",
                              "Unemployment rate" = "unemployment_rate",
                              "GDP per capita" = "GDP_per_capita",
                              "Homicide rate" = "homicide_rate",
                              "Population" = "population",
                              "Total fisheries" = "total_fisheries"
                            ), 
                            selected = "GDP_per_capita"
                          ),
                          br(),
                          textOutput("text3")
                   ),
                   column(9,
                          markdown("### Scatter plot of countries based on selected indicators"),
                          withSpinner(plotlyOutput("Scatter"), type = 6, hide.ui = F, color = "darkred"),
                          br(),
                          markdown("The scatter plot shows the relationship between two indicators across different countries. Each point represents a country, with its position showing the values of these two indicators. The colors of the points indicate the geographical region of each country, making it easier to compare countries within the same region."),
                          markdown("To keep things consistent, all data (except the number of attacks) is from 2016. This helps you spot patterns without worrying about different years messing things up. The number of attacks changes based on the years you select, letting you see how different factors relate to pirate activity over time. This makes it easy to explore how these factors and piracy incidents connect across different periods.")
                          
                   )
                 )
                 )

uinfo <- fluidPage(titlePanel("info"),
                   fluidRow(
                     column(12,
                            h3("Pirate attacks analysis: introduction"),
                            p("Our app gives a complete look at pirate attacks from 1993 to 2020 with four easy-to-use tabs. The first tab has a bar chart showing how often attacks happened each year. The second tab has interactive maps that show where the attacks took place. The other tabs include detailed bar charts and scatter plots to help you dive deeper into the factors behind these attacks. You can filter the data by specific dates to see trends over time, making it a convenient tool for exploring piracy data."),
                            plotlyOutput("bar_years")
                     )
                   )
                   )

ui <- navbarPage(
  title = "Pirate attacks",
  tabPanel("info", uinfo, icon = icon("circle-info")),
  tabPanel("Map", ui1, icon = icon("map-location-dot")),
  tabPanel("Types of attacks", ui2, icon = icon("chart-column")),
  tabPanel("Correlations", ui3, icon = icon("magnifying-glass-chart")),
  theme = bs_theme(bootswatch = "lux"),
  header = fluidPage(
    fluidRow(
      chooseSliderSkin(skin = "Shiny", color = "darkred"), #73B3C6 - niebieski
      sliderInput("slider1", "Filter attacks by a year range:",
                  min = 1993, max = 2020, value = c(1993, 2020), sep = "")
    )
  ),
  footer = shiny::HTML("
                 <footer class='text-center text-sm-start' style='width:100%;'>
                 <hr>
                 <p class='text-center' style='font-size:12px;'>
                   © 2024 Copyright:
                   <a class='text-dark' href='https://www.youtube.com/watch?v=dQw4w9WgXcQ&pp=ygUXbmV2ZXIgZ29ubmEgZ2l2ZSB5b3UgdXA%3D'>Dziobaks Entertainment</a>
                 </p>
                 </footer>
                 ")
)

runApp(shinyApp(ui, server))