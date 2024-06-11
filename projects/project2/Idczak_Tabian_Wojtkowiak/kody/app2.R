library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr) 
library(tidyr)
library(leaflet)
library(stringi)
library(sf)
library(geosphere)


## OLA -------------------------------------------------------------------------
data1_ola <- read.csv("https://raw.githubusercontent.com/olaidczak/projekt-2/main/tfl-journeys-type-new.csv")
data_taps <- read.csv("https://raw.githubusercontent.com/olaidczak/projekt-2/main/london-data.csv")
data_stations <- read.csv("https://raw.githubusercontent.com/olaidczak/projekt-2/main/stations.csv")

data_taps1 <- data_taps %>% 
  mutate(TravelDate = paste0(str_sub(TravelDate,1,4),"-",str_sub(TravelDate,5,6),"-",str_sub(TravelDate,7,8))) %>% 
  mutate(TravelDate = as.Date(TravelDate, "%Y-%m-%d")) %>% 
  mutate(taps = EntryTapCount+ExitTapCount)

data2_ola <- data1_ola %>% 
  mutate(date = as.Date(Period.beginning, "%d-%m-%y"))

data3_ola <- data2_ola %>% 
  pivot_longer(
    cols = c('Bus', 'Underground', 'DLR','Tram','Overground','London.Cable.Car','TfL.Rail'),
    names_to = "transport.type") 


## OLIWIA -------------------------------------------------------------------------
miesiace <- read.csv("https://raw.githubusercontent.com/olaidczak/projekt-2/main/miesiace1.csv", sep = ";", dec = ".")
bus <- read.csv("https://raw.githubusercontent.com/olaidczak/projekt-2/main/technical-analysis-data-bus-services.csv", sep = ";", dec = ".")
undrgrnd <- read.csv("https://raw.githubusercontent.com/olaidczak/projekt-2/main/lu-average-monthly-temperatures.csv", sep = ",", dec = ".")

buses <- bus %>%
  pivot_longer(
    cols <- c('London', 'Other.English.metropolitan.areas', 'English.non.metropolitan.areas'),
    names_to = 'places') 

buses$value <- str_replace_all(buses$value, " ", "")
buses$places <- stri_replace_all_fixed(buses$places, ".",' ')

under <- undrgrnd %>%
  pivot_longer(
    cols <- c("Sub.surface_lines", "Waterloo_and_City", "Victoria", "Piccadilly", "Northern", "Jubilee", "Central", "Bakerloo"),
    names_to = 'stops')

## MAKS -------------------------------------------------------------------------
data1 <- read.csv('https://raw.githubusercontent.com/olaidczak/projekt-2/main/Tab12_2015-2023.csv')
data2 <- read.csv('https://raw.githubusercontent.com/olaidczak/projekt-2/main/Tab10_3-2023.csv')

colors <- c("#1509bb", "#6845c7", "#9678d1", "#beacda", "#f7bbae", "#ff917c", "#ff614c", "#ff001c")



# Data preparation for MAP 1 ------------------------------------------
data_cities <- data1 %>%
  filter(report_period == as.factor(report_period)) %>% 
  group_by(report_period, foreign_apt) %>% 
  summarise(Total_London = sum(Total)) %>% 
  group_by(report_period) %>%
  top_n(n = 50, wt= Total_London) %>% 
  arrange(report_period, desc(Total_London)) %>% 
  ungroup()

airports <- data_cities %>%
  group_by(report_period) %>% 
  top_n(n = 20, wt = Total_London) %>% 
  ungroup() %>% 
  distinct(foreign_apt)

lat <- c(
  53.4214, 52.3676, 25.2532, 40.6413, 40.4168, 41.2975, 46.2380, 55.6180,
  41.8000, 36.7213, 50.1109, 47.4647, 49.0097, 48.3537, 33.9416, 38.7742,
  22.3080, 39.5696, 59.6498, 43.7102, 47.4979, 37.0144, 38.2822, 25.2736, 
  44.5720, 42.6977, 37.9838, 41.0082, 28.2916
)

lon <- c(
  -6.2701,  4.9041, 55.3657, -73.7781, -3.7038,  2.0833,    6.1431, 12.6561,
  12.2500, -4.4213,  8.6821,   8.5493,  2.5479, 11.7750, -118.4085, -9.1346, 
  113.9184, 2.6466, 17.9230,   7.2619, 19.2551, -7.9653,   -0.5582, 51.6089,
  26.1024, 23.3219, 23.7275,  28.2916, -16.6291
)

data_to_add <- data.frame(airports, lat, lon)
data_cities2 <- data_cities %>% 
  left_join(data_to_add, by = "foreign_apt")

# Współrzędne Londynu
londyn <- data.frame(
  airports = "London",
  lon = -0.1276,
  lat = 51.5074,
  passengers = NA
)

# Funkcja do normalizacji grubości linii
normalize <- function(x, min_width = 0.05, max_width = 5) {
  (x - min(x)) / (max(x) - min(x)) * (max_width - min_width) + min_width
}

# Data preparation for MAP 2 ------------------------------------------
airports_london_coordinates<- data.frame(
  airport = c("GATWICK", "HEATHROW", "LONDON_CITY", "LUTON", "SOUTHEND", "STANSTED"),
  lat = c(51.1537, 51.4694, 51.5052, 51.8747, 51.5714, 51.885),
  lon = c(-0.1821, -0.4514, 0.0553, -0.368, 0.6952, 0.235)
)
# Data preparation for Plot  --------------------------------------------

data_airports_london_passengers <- data2 %>% 
  filter(prt_apt_grp_name == "London Area Airports") %>% 
  select(-c(1,2,3,4,5,18)) %>% 
  rename(Airport = rpt_apt_name,
         "2013" = Y1, "2014" = Y2, "2015" = Y3, "2016" = Y4, "2017" = Y5, "2018" = Y6,
         "2019" = Y7, "2020" = Y8, "2021" = Y9, "2022" = Y10, "2023" = Y11)

# Because of some problems with transposition of the data frame, data will be 
# entered manually

Year <- c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", 
          "2021", "2022", "2023")
GATWICK <- c(35428548,38093878,40260068,43114888,45553837,46081327,46574786,
             10171867,6260072,32831088,40894242)
HEATHROW <- c(72331690,73371096,74953708,75671863,77987524,80100311,80886589,
              22109550,19392178,61596618,79149042)
LONDON_CITY <- c(3379753,3647824,4319301,4538735,4530439,4820403,5122271,908105,
                 720580,3009313,3429684)  
LUTON <- c(9693487,10481501,12262581,14642282,15989225,16766552,18213901,5550401,
           4673656,13322236,16399866)
SOUTHEND <-c(969912,1102260,900634,874411,1091616,1480139,2035535,401143,
             94367,89361,146072)
STANSTED <- c(17848871,19958047,22513443,24318395,25902618,27995121,28124292,
              7536869,7145802,23289652,27951116)
airports_london_passengers <- data.frame(Year, GATWICK, HEATHROW, LONDON_CITY,
                                         LUTON, SOUTHEND, STANSTED)
## -----------------------------------------------------------------------------

ui1 <- fluidPage(
  titlePanel("LONDON AIRPORTS"),
  
  fluidRow(
    column(
      width = 4,
      leafletOutput("map2")
    ),
    
    
    column(
      width = 2,
      textOutput("text1"),
      br(),
      selectInput("airport", "Select Airport",
                  choices = c("GATWICK", "HEATHROW", "LONDON_CITY", "LUTON", "SOUTHEND", "STANSTED"),
                  selected = "GATWICK"),
      br(),
      textOutput("text2"),
      style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:24px'
    ),
    
    
    column(
      width = 6,
      actionButton("select_all", "Select All Airports"),
      plotlyOutput("plot")
      
    )   
  ),
  
  #div("TOP INTERNATIONAL DESTINATIONS", style = "font-size: 24px; margin-bottom: 24px;"),
  h3('TOP INTERNATIONAL DESTINATIONS'),
  
  fluidRow(
    column(
      width = 2,
      selectInput("year", "Select Year",
                  choices = c("2015", "2016", "2017", "2018", "2019", "2020", "2021",
                              "2022", "2023")),
      textOutput("text3"),
      
    ),
    
    column(
      width = 3,
      dataTableOutput("table")
      
    ),
    
    column(
      width = 7,
      leafletOutput("map")
      
    ))
)

ui2 <- fluidPage(
  titlePanel("Different specified transportation means"),
  textOutput('text'),
  fluidRow(
    column(3,
           h3("Bus journeys"),
           p("The first plot shows how number of bus journeys was changing through years.
             There are three areas to choose from. There is an option to choose all three,
             which allows to compare all the areas. Every year London is at the
             forefront of number of bus trips. In general, the count fluctuate between one billion
             (in other English metropolitan areas) and over two billions (in London, no surprise here)."),
    ),
    column(7, 
           style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:40px',
           plotlyOutput("points2")),
    column(2,
           #wellPanel(
           checkboxGroupInput("Places",
                              "Choose places:",
                              choiceValues =   c('London', 'Other English metropolitan areas', 'English non metropolitan areas'),
                              choiceNames =  c('London', 'Other English metropolitan areas', 'English non metropolitan areas'),
                              selected = c("London", 'Other English metropolitan areas'))
           #)
    )
  ),
  fluidRow(
    h3("bicycle hires"),
    column(3,
           p(paste("The following plot depicts a number of bicycle hires in London through the months. On the right from this section
           you can choose as many years as you want. Our chart will show years (every in different colour) divided
           by months. By hovering over on specified place on the chart you can see the year and the number of bicycle
           hired in the choosen month of that year. As you can prefigure, people rent a lot more bikes 
           in summer (july and august) than in winter months (december, january and february). There many
           other dependents to be discovered..."))),
    column(7, 
           plotlyOutput("points1")),
    column(2, 
           #wellPanel(
           checkboxGroupInput("Years",
                              "Choose years:",
                              choiceValues =  unique(miesiace$Year),
                              choiceNames =  c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"),
                              selected = c("2010",'2014',"2020", "2022"))
           #)
    )
  ),
  fluidRow(
    h3('underground stations'),
    p("The last plot in this section shows the dependence of underground station and the average temperature (every month seperately).
           Below, you can pick underground stops and a month, in which the average temperatures interest you. 
           On the plot located under the text section you can see line charts (each for every chosen station, in different colours - you 
           would find assingment in a legend). We can see that in winter months in sub surface lines the temperature is way lower than in the rest of
           stations. Another whizzy remark is that in july 2020 in every underground stop was an apparent fall of
           temperature in comparison to previous years."),
    column(3, 
           #wellPanel(
           selectInput("Month",
                       "Choose month:",
                       unique(under$Month),
                       selected = c("February")),
           checkboxGroupInput("Stops",
                              "Choose stops:",
                              choiceValues = c("Sub.surface_lines", "Waterloo_and_City", "Victoria", "Piccadilly", "Northern", "Jubilee", "Central", "Bakerloo"),
                              choiceNames = c("Sub surface lines", "Waterloo and City", "Victoria", "Piccadilly", "Northern", "Jubilee", "Central", "Bakerloo"),
                              selected = c("Victoria", "Northern", "Bakerloo"))
           #)
    ),
    column(8, 
           plotlyOutput("points3"))
  ))

ui3 <- fluidPage(
  tags$head(tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #868687;
                                                  border-top: 1px solid #868687;
                                                  border-bottom: 1px solid #868687;}
                           .irs-from, .irs-to, .irs-single {background: #868687;}
                           .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single{
                             background-color:#868687;}'
  ))
  ),
  
  titlePanel("Transport in London"),
  fluidRow(
    h3("Journeys by travel mode"),
    column(3, 
           textOutput("text1_ola")),
    column(7, 
           style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:40px',
           plotlyOutput("pointPlot2_ola",width = "100%")),
    column(2,
           #wellPanel(
           checkboxGroupInput("typetransport",
                              "Select modes of transportation:",
                              choiceValues = c('Bus', 'Underground', 'DLR','Tram','Overground','London.Cable.Car','TfL.Rail'),
                              choiceNames = c('Bus', 'Underground', 'DLR','Tram','Overground','London Cable Car','TfL Rail'),
                              selected = c('Bus', 'Underground')),
           sliderInput("timeperiod",
                       "Select time period: ",
                       min = data3_ola$date[1],
                       max = data3_ola$date[1274], 
                       value = c(data3_ola$date[1], data3_ola$date[1274]),
                       timeFormat="%d-%m-%y")
           #)
    )
  ),
  fluidRow(
    h3("Most popular stations"),
    textOutput("text2_ola"),
    column(6, 
           style='padding-left:0px; padding-right:7px; padding-top:20px; padding-bottom:0px',
           plotlyOutput("barPlot_ola")),
    column(4, 
           style='padding-left:7px; padding-right:7px; padding-top:20px; padding-bottom:7px',
           leafletOutput("map_ola", width = "100%", height = "100%")),
    column(2,
           style='padding-left:10px; padding-right:10px; padding-top:40px; padding-bottom:0px',
           #wellPanel(
           numericInput("num", 
                        "Select number of stations:", 
                        value = 10, 
                        min = 1, 
                        max = 20)
           #)
    )
  ),
)

app_ui <- navbarPage(
  title = "London",
  tabPanel("Airports", ui1),
  tabPanel("Means of transport", ui3),
  tabPanel("Various statistics", ui2),
  theme = bslib::bs_theme(bootswatch = "lux"),
  footer = shiny::HTML("
                  <footer class='text-center text-sm-start' style='width:100%;'>
                  <hr>
                  <p class='text-center' style='font-size:12px;'>
                    <span style='font-weight: bold;'>Authors:</span> Aleksandra Idczak, Maksymilian Tabian, Oliwia Wojtkowiak;
                    Sources: 
                    <a class='text-dark' href='https://tfl.gov.uk/info-for/open-data-users/our-open-data'>tfl</a>,
                    <a class='text-dark' href='https://www.caa.co.uk/data-and-analysis/uk-aviation-market/airports/uk-airport-data/'>ukcaa</a>
                  </p>
                  </footer>
                  ")
)

server <- function(input, output) {
  
  # OLA ------------------------------------------------------------------------
  output$text1_ola <- renderText({
    paste("This plot explores journey counts across different transportation modes. You can choose from seven different means of transport and compare them over a selected period of time. It comes as no surprise that the most popular networks are Bus and Tube, leaving the rest far behind. Moreover, you can discover how the number of journeys changes throughout the year and which months are the busiest."
    )
  })
  
  output$text2_ola <- renderText({
    paste("This section highlights the most popular stations in the London railway system (Underground, Overground, TfL Rail, DLR) from 2019 to 2024. You can see the busiest stations on the map - as expected most of them are located in Central London.")
  })
  
  output$pointPlot2_ola <- renderPlotly({
    fig <- plot_ly(data3_ola %>% 
                     filter(transport.type %in% c(input$typetransport))%>% 
                     filter(date >= input$timeperiod[1] & date <= input$timeperiod[2]),
                   x = ~ date,
                   y = ~ value,
                   color = ~ transport.type,
                   colors = c("#1509bb",
                              "#6845c7",
                              "#9678d1",
                              "#beacda",
                              "#f7bbae",
                              "#ff917c",
                              "#ff614c",
                              "#ff001c"),
                   type = 'scatter',
                   mode = 'lines',
                   textposition = "auto",
                   text = ~ transport.type,
                   hovertemplate = paste(
                     "<b> %{text} </b><br>",
                     "%{x}<br>",
                     "Journey count: %{y} millons",
                     "<extra></extra>"
                   )
    ) %>% 
      layout(title = "Journey counts by travel mode",
             yaxis = list(title = "Journey count (millions)"),
             xaxis = list(title = "Year"))
  })
  
  output$barPlot_ola <- renderPlotly({
    df1 <- data_taps1 %>% 
      group_by(Station) %>% 
      summarise(sum = sum(taps)) %>% 
      arrange(-sum) %>% 
      mutate(id = row_number()) %>% 
      head(input$num[1])
    plot_ly(df1,
            x = ~ Station,
            y = ~ sum,
            type = 'bar',
            marker = list(color = '#1509bb'),
            textposition = "auto",
            hovertemplate = paste(
              "<b>Station: %{x} </b><br>",
              "<b>Number of passengers</b>: %{y}",
              "<br><b>Ranking:</b> ",df1$id,
              "<extra></extra>")) %>% 
      layout(title = "Stations ranked by number of passangers (2019-2024)",
             xaxis = list(title = "Station", categoryorder = "total descending"),
             yaxis = list(title = "Number of passangers"))
  })
  
  output$map_ola <- renderLeaflet({
    df1 <- data_taps1 %>% 
      group_by(Station) %>% 
      summarise(sum = sum(taps)) %>% 
      arrange(-sum) %>% 
      mutate(id = row_number()) %>% 
      head(input$num[1])
    
    df2 <- data_stations %>% 
      filter(NAME %in% df1$Station)
    
    df3 <- left_join(df1, df2, by = join_by(Station == NAME))
    
    df3 %>% 
      leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(
        lng = ~ x,
        lat = ~ y,
        popup = paste(
          "<b>Station:</b>", df3$Station,
          "<br><b>Number of passengers:</b> ", round(df3$sum/1000000, digits = 3),"million",
          "<br><b>Ranking:</b> ",df3$id),
        radius = 3,
        color = '#ff001c',
        fill = TRUE,
        opacity = 1
      )
    # addMarkers(lng = ~ x,
    #            lat = ~ y,
    #            popup = paste(
    #              "<b>Station:</b>", df3$Station,
    #              "<br><b>Number of passengers:</b> ", df3$sum,
    #              "<br><b>Ranking:</b> ",df3$id))
    
  })
  # OLIWIA ---------------------------------------------------------------------
  output$points2 <- renderPlotly({
    fig <-plot_ly(buses <- buses %>% 
                    filter(buses$places %in% input$Places),
                  x = ~ buses$Years,
                  y = ~ as.numeric(buses$value),
                  color = ~ buses$places,
                  colors = c(
                    '#1509bb',
                    "#f7bbae",
                    "#ff001c"),
                  type = "bar") %>% 
      layout(
        title = "Number of bus passengers journeys (in billions)", 
        xaxis = list(title = "Month"), 
        yaxis = list(title = 'Number of journeys'))
    
  })
  output$points1 <- renderPlotly({
    fig <-plot_ly(miesiace <- miesiace %>%
                    filter(miesiace$Year %in% input$Years),
                  x = ~ miesiace$Month,
                  y = ~ miesiace$Number.of.Bicycle.Hires.Monthly,
                  color = ~ as.character(miesiace$Year),
                  colors = c("#1509bb",
                             "#6845c7",
                             "#9678d1",
                             "#beacda",
                             "#f7bbae",
                             "#ff917c",
                             "#ff614c",
                             "#ff001c"),
                  type = "bar") %>%
      layout(
        title = "Monthly bicycle hires (in millions)",
        xaxis = list(title = 'Month'),
        yaxis = list(title = 'Number of hires'),
        barmode = "stack")
  })
  
  output$points3 <- renderPlotly({
    fig <-plot_ly(under <- under %>% 
                    filter(under$stops %in% input$Stops,
                           under$Month %in% input$Month),
                  x = ~ under$Year,
                  y = ~ under$value,
                  color = ~ under$stops,
                  colors = c("#1509bb",
                             "#6845c7",
                             "#9678d1",
                             "#beacda",
                             "#f7bbae",
                             "#ff917c",
                             "#ff614c",
                             "#ff001c"),
                  type = "scatter", mode ="lines+markers") %>%  
      layout(
        title = "Average temperature at London stops in particular months (in Celsius degrees)", 
        xaxis = list(title = "Year", showgrid = F), 
        yaxis = list(title = 'Temperature'),
        scale_x_discrete(name = "Year", breaks = 1)
      ) 
    
  })
  # MAKS
  # MAP 1 ------------------------------------------------------------------
  
  # Reactive expression for the data subsetted to what the user selected
  Map_data <- reactive({
    req(input$year)
    data_cities2 %>%
      filter(report_period == input$year) %>% 
      mutate(width = normalize(Total_London))
  })
  
  
  # Aspects of the map that won't need to change dynamically
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% 
      setView(lng = 0, lat = 50, zoom = 4) %>% 
      addCircleMarkers(
        lng = londyn$lon,
        lat = londyn$lat,
        label = "London",
        radius = 3,
        color = colors[8],
        fill = TRUE,
        opacity = 1
      )
  })
  
  # Incremental changes to the map 
  observe({
    map_data <- Map_data()
    proxy <- leafletProxy("map")
    proxy %>% clearShapes() %>% clearMarkers()
    
    for (i in 1:20) {
      lines <- gcIntermediate(c(londyn$lon, londyn$lat), 
                              c(map_data$lon[i], map_data$lat[i]), 
                              n = 180, addStartEnd = TRUE, sp = TRUE)
      
      proxy %>%
        addPolylines(data = lines,
                     weight = map_data$width[i],
                     color = colors[1],
                     opacity = 0.4) %>%
        addCircleMarkers(
          lng = map_data$lon[i],
          lat = map_data$lat[i],
          label = paste0(map_data$foreign_apt[i], ": ", map_data$Total_London[i], " passengers"),
          radius = 2,
          color = colors[6],
          fill = TRUE,
          opacity = 1
        )
    }
  })
  
  
  # MAP 2 -------------------------------------------------------------------
  output$map2 <- renderLeaflet({
    leaflet(airports_london_coordinates) %>%
      addTiles() %>%
      addLabelOnlyMarkers(
        lng = ~lon, 
        lat = ~lat, 
        label = ~airport,
        labelOptions = labelOptions(noHide = TRUE, style = list("font-size" = "8px"), offset = c(0, -10))
      ) %>% 
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = 1.5,
        color = colors[1],
        fill = TRUE,
        opacity = 1
      )
    
  })
  
  # ?addLabelOnlyMarkers
  # Table -------------------------------------------------------------------
  
  output$table <- renderDataTable({
    data_cities %>% 
      filter(report_period == input$year) %>% 
      select(foreign_apt, Total_London) %>% 
      rename(Airport = foreign_apt, Passengers = Total_London)
  }, options = list(scrollY = "300px", 
                    scrollCollapse = TRUE,  
                    paging = FALSE,
                    lengthChange = FALSE,
                    searching = FALSE)
  )
  
  
  # Plot --------------------------------------------------------------------
  
  all_selected <- reactiveVal(FALSE)
  
  observeEvent(input$select_all, {
    all_selected(TRUE)
  })
  
  observeEvent(input$airport, {
    all_selected(FALSE)
  })
  
  output$plot <- renderPlotly({
    if (all_selected()) {
      plot <- plot_ly(data = airports_london_passengers, x = ~Year, y = ~GATWICK,
                      name = 'Gatwick', type = "scatter", mode = "lines+markers",
                      line = list(color = colors[1], dash = "dash"),
                      marker = list(color = colors[1])) %>%
        add_trace(y = ~HEATHROW, name = 'Heathrow',
                  line = list(color = colors[2], dash = "dash"),
                  marker = list(color = colors[2])) %>%
        add_trace(y = ~LONDON_CITY, name = 'London City',
                  line = list(color = colors[3], dash = "dash"),
                  marker = list(color = colors[3])) %>%
        add_trace(y = ~LUTON, name = 'Luton',
                  line = list(color = colors[4], dash = "dash"),
                  marker = list(color = colors[4])) %>%
        add_trace(y = ~SOUTHEND, name = 'Southend',
                  line = list(color = colors[5], dash = "dash"),
                  marker = list(color = colors[5])) %>%
        add_trace(y = ~STANSTED, name = 'Stansted',
                  line = list(color = colors[6], dash = "dash"),
                  marker = list(color = colors[6]))
    } else {
      selected_color <- colors[which(c("GATWICK", "HEATHROW", "LONDON_CITY", "LUTON", "SOUTHEND", "STANSTED") == input$airport)]
      plot <- plot_ly(data = airports_london_passengers, x = ~Year, y = ~get(input$airport),
                      name = input$airport, type = "scatter", mode = "lines+markers",
                      line = list(color = selected_color, dash = "dash"),
                      marker = list(color = selected_color))
    }
    
    plot <- plot %>%
      layout(title = list(text = "Terminal Passengers at London airports 2013-2023", y = 0.98),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Passengers in millions"))
    
    return(plot)
  })
  
  # Text --------------------------------------------------------------------
  
  output$text1 <- renderText({
    "There are six main airports in London. Choose one to find out more about it."
  })
  
  output$text2 <- renderText({
    airport <- switch(input$airport,
                      "GATWICK" = "Gatwick Airport is one of the busiest airports in the United Kingdom,
      located approximately 30 miles south of Central London. It serves as a major hub for both domestic and international flights.
      With two terminals, Gatwick offers a wide range of amenities including shops, restaurants, and lounges for travelers.",
                      
                      "HEATHROW" = "Heathrow Airport, situated to the west of London, is the largest airport in the UK
       and one of the busiest in the world. It operates with four terminals, serving millions of passengers
       annually with flights to destinations worldwide. Heathrow is known for its extensive facilities, including
       shopping centers, dining options, and premium lounges.",
                      
                      "LONDON_CITY" = "London City Airport is uniquely located in the heart of London, making it a convenient choice for business travelers.
       Despite its smaller size compared to other airports in London, it offers flights to various European destinations. London City Airport
       is known for its efficiency and quick access to the city center.",
                      
                      "LUTON" = "Luton Airport, located north of Central London, primarily serves low-cost airlines offering flights across Europe.
       Despite being smaller than Heathrow and Gatwick, it has seen significant growth in recent years. Luton Airport provides essential
       facilities for travelers, including shops, cafes, and transportation links to London.",
                      
                      "SOUTHEND" = "Southend Airport, situated in Essex to the east of London, offers a more relaxed alternative to the larger airports
       in the region. It serves primarily domestic and European destinations, catering to both leisure and business travelers.
       Southend Airport boasts modern facilities and convenient transport connections to London.",
                      
                      "STANSTED" = "Stansted Airport, located northeast of London, is a major hub for budget airlines serving European destinations.
       Despite its distance from the city center, Stansted offers efficient transport links, including train services to London.
       The airport features a range of amenities, including shopping outlets, restaurants, and executive lounges."
    )
    return(airport)
  })
  
  
  output$text3 <- renderText({
    "The table shows the 50 most popular airports in a chosen year in terms of connections to and from London.
     The map in turn shows the 20 most popular destinations. The thickness of the line indicates the popularity of a particular route."
  })
  
}

runApp(shinyApp(app_ui, server))