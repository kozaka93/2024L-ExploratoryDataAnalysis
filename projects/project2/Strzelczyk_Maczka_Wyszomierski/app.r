####################################
# Załadowanie potrzebnych pakietów #
####################################

library(shiny)
library(dplyr)
library(plotly)
library(shinycssloaders)
library(forcats)
library(leaflet)
library(stringr)
library(ggmap)
library(geojsonio)
library(lubridate)
library(tidyverse)
library(shinydashboard)
library(dashboardthemes)

# Poniższe dane pochodzą ze strony:
# https://data.cityofchicago.org/Transportation/Taxi-Trips-2023/e55j-2ewb/about_data
df <- read.csv("C:/Users/acer/Documents/taxitrips03052023.csv")

chicago <- geojson_read("https://raw.githubusercontent.com/RandomFractals/ChicagoCrimes/master/data/chicago-community-areas.geojson", what = "sp")

################
# dane Łukasza #
################

df_lukasz <- df %>% 
  mutate(date_without_hour = as.Date(str_extract(Trip.Start.Timestamp, 
                                                 "[0-9]{2}\\/[0-9]{2}\\/[0-9]{4}"), 
                                     format = "%m/%d/%Y"))

##############
# dane Julki #
##############

taxitrips <- df %>% 
  select(-Taxi.ID,-Trip.ID, -Pickup.Centroid.Location, -Dropoff.Centroid..Location)

df5 <- parse_date_time(taxitrips$Trip.Start.Timestamp, '%m/%d/%Y %I:%M:%S %p')

tips_by_day <- data.frame(taxitrips, weekday = wday(df5, week_start = 1)) %>% 
  select(Tips, Fare, Payment.Type, weekday)


df4 <- parse_date_time(taxitrips$Trip.Start.Timestamp, '%m/%d/%Y %I:%M:%S %p')

df4 <- data_frame(
  date_time = df4,
  day = day(df4),
  month = month(df4),
  hour = hour(df4),
  minute = minute(df4),
  weekday = wday(df4, week_start = 1)
)

p <- df4 %>% 
  group_by(hour) %>% 
  summarise(n = n())


pon <- df4 %>% 
  filter(weekday == 1) %>% 
  group_by(hour) %>% 
  summarise(pon = n())
wt <- df4 %>% 
  filter(weekday == 2) %>% 
  group_by(hour) %>% 
  summarise(wt = n())
sr <- df4 %>% 
  filter(weekday == 3) %>% 
  group_by(hour) %>% 
  summarise(sr = n())
czw <- df4 %>% 
  filter(weekday == 4) %>% 
  group_by(hour) %>% 
  summarise(czw = n())
pt <- df4 %>% 
  filter(weekday == 5) %>% 
  group_by(hour) %>% 
  summarise(pt = n())
sb <- df4 %>% 
  filter(weekday == 6) %>% 
  group_by(hour) %>% 
  summarise(sb = n())
ndz <- df4 %>% 
  filter(weekday == 7) %>% 
  group_by(hour) %>% 
  summarise(ndz = n())

tydzien <- pon %>% 
  left_join(wt) %>% 
  left_join(sr) %>% 
  left_join(czw) %>% 
  left_join(pt) %>% 
  left_join(sb) %>% 
  left_join(ndz) %>% 
  left_join(p)

##############
# dane Marka #
##############

# Poniżse dane pochodzą ze strony:
# https://data.cityofchicago.org/Community-Economic-Development/Public-Passenger-Vehicle-Licenses-Taxis-Only/gcze-gasw/about_data
vehicles <- read.csv("C:/Users/acer/Downloads/Public_Passenger_Vehicle_Licenses_-_Taxis_Only_20240524.csv", header = TRUE)

##############################################################################################################################

ui <- dashboardPage(
  dashboardHeader(title = "Taxis in Chicago"),
  dashboardSidebar(sidebarMenu(
    menuItem("Information page", tabName = "info_page", icon = icon("info")),
    menuItem("Taxi cars", tabName = "Marek", icon = icon("taxi")),
    menuItem("Taxi trips in 2023", tabName = "Julka", icon = icon("taxi")),
    menuItem("Taxis in each district", tabName = "Lukasz", icon = icon("taxi"))
  )),
  dashboardBody(shinyDashboardThemes(
    theme = "blue_gradient"
  ),
  tags$style(HTML("
    .box.box-solid.box-primary>.box-header {
      color:#000000;
      background:#0a87a6
                        }
    
    .box.box-solid.box-primary{
    border-bottom-color:#0a87a6;
    border-left-color:#0a87a6;
    border-right-color:#0a87a6;
    border-top-color:#0a87a6;
    }")),
  tabItems(
      tabItem(tabName = "info_page",
              fluidRow(
                column(width = 12,
                  box(
                    title  = "Project about transport",
                    solidHeader = TRUE,
                    status = "primary",
                    width = NULL,
                    div(
                      p("We are second-year students of Mathematics and Data Analysis and this project was done for the subject Introduction to exploratory data analysis. We analyzed data about cabs in Chicago."),
                      br(),
                      p("Data comes from: ",
                        a(href = "https://data.cityofchicago.org/Transportation/Taxi-Trips-2013-2023-/wrvz-psew/about_data", "Chicago Taxi Trips Data")
                      ),
                      br(),
                      p("We hope you like it"),
                      p("Authors:"),
                      p("Julia Strzelczyk"),
                      p("Marek Mączka"),
                      p("Łukasz Wyszomierski"))))),
              fluidRow(
                box(
                  solidHeader = TRUE,
                  status = "primary",
                  width = 12,
                  div(imageOutput("logo_mini"), style="text-align: center;")
                ))),
      tabItem(tabName = "Marek",
              fluidRow(
                column(width = 4,
                  box(
                    title = "Type of fuel",
                    solidHeader = TRUE,
                    status = "primary",
                    width = NULL,
                    selectInput(
                      inputId = "fuel",
                      label = "Select type of fuel:",
                      choices = NULL
                  )),
                  box(width = NULL,
                      solidHeader = TRUE,
                      status = "primary",
                      "The chart shows the years of production of 
                      wheelchair-accessible cabs. The number of these vehicles 
                      varies depending on the year of production, with no clear 
                      trend of improvement or deterioration in this regard.
                      It can be noted that the highest number of wheelchair-accessible 
                      cabs operating on the streets of Chicago was produced in 2019 
                      and 2020, 76 and 80 vehicles, respectively. In other years, 
                      the number of such vehicles is sometimes much smaller. 
                      In 2013 and 2015, the number is minimal, not exceeding
                      a few vehicles per year.") ),
                column(width = 8,
                  box(
                    title = "Wheelchair accessible taxis",
                    solidHeader = TRUE,
                    status = "primary",
                    width = NULL,
                    shinycssloaders::withSpinner(
                    plotlyOutput("wheelchairPlot"),
                    type = getOption("spinner.type", default = 1),
                    color = getOption("spinner.color", default = "#0a87a6"),
                    size = getOption("spinner.size", default = 1)
                ))),
                column(width = 8,
                  box(
                    title = "Distribution of vehicles' Model Year",
                    solidHeader = TRUE,
                    status = "primary",
                    width = NULL,
                    shinycssloaders::withSpinner(
                    plotlyOutput("modelYearPlot"),
                    type = getOption("spinner.type", default = 1),
                    color = getOption("spinner.color", default = "#0a87a6"),
                    size = getOption("spinner.size", default = 1)
                ))),
                column(width = 4,
                  box(width = NULL,
                      solidHeader = TRUE,
                      status = "primary",
                      "The graph shows the distribution of the 
                      number of cabs in Chicago by year of manufacture, 
                      where the horizontal axis represents years and the vertical 
                      axis represents the number of vehicles. Until around 2005, 
                      the number of vehicles was negligible. Starting in 2010, 
                      the number of vehicles begins to increase significantly, 
                      culminating between 2011 and 2015. After 2015, the number 
                      of vehicles begins to decrease significantly, and from 2020 
                      it is again low. It is also worth seeing the distribution 
                      of cabs with a set fuel type. It can then be seen, perhaps 
                      surprisingly, that gasoline-powered cabs in Chicago are, 
                      on average, newer than the hybrid ones.") 
                       ))),
      tabItem(tabName = "Julka",
              fluidRow(
                valueBox(paste(round(mean(df_lukasz$Fare, na.rm = TRUE),2), "$"), "Mean trip fare",
                         color = "teal", icon = icon("taxi")),
                valueBox(paste(round(mean(df_lukasz$Trip.Miles*1.609344, na.rm = TRUE),2), "km"),"Mean trip distance", 
                         color = "teal", icon = icon("taxi")),
                valueBox(paste(round(mean(df_lukasz$Tips, na.rm = TRUE),2), "$"),"Mean tip",
                         color = "teal", icon = icon("taxi"))
              ),
              fluidRow(
                column(width = 3,
                  box(width = NULL, 
                      title ='',
                      solidHeader = TRUE,
                      status = "primary",
                      selectInput("week_day",
                                  "Weekday:",
                                  choices = c("All","Monday","Tuesday","Wednesday",
                                              "Thursday", "Friday", "Saturday", "Sunday"))),
                  box(width = NULL, 
                      solidHeader = TRUE,
                      status = "primary",
                      "The chart shows the distribution of taxi trips 
                      by hour on selected day of the week. The busiest time of day is the afternoon. 
                      The peak period for trips is at 5 PM, likely due to people returning 
                      from work. The lowest number of trips occurs at night.")),
                column(width = 9,
                  box(
                    title = "Number of taxi trips on selected day",
                    solidHeader = TRUE,
                    status = "primary",
                    width = NULL,
                    shinycssloaders::withSpinner(
                    plotlyOutput("trips_per_hour"),
                    type = getOption("spinner.type", default = 1),
                    color = getOption("spinner.color", default = "#0a87a6"),
                    size = getOption("spinner.size", default = 1)
                  )))),
              fluidRow(
                column(width = 9,
                  box(
                    title = "Tip percentage",
                    solidHeader = TRUE,
                    status = "primary",
                    width = NULL,
                    shinycssloaders::withSpinner(
                    plotlyOutput("tip_percentage"),
                    type = getOption("spinner.type", default = 1),
                    color = getOption("spinner.color", default = "#0a87a6"),
                    size = getOption("spinner.size", default = 1)
                  ))),
                column(width = 3,
                  box(width = NULL,
                      solidHeader = TRUE,
                      status = "primary",
                      "The chart shows the average tip percentage left by passengers 
                      on different days of the week. Analyzing this data, we can see 
                      that passengers' generosity varies depending on the day of the 
                      week. Weekdays, especially Tuesdays, Wednesdays and Thursdays, 
                      are characterized by high tips. Saturdays, despite being a day 
                      off, do not encourage greater generosity from passengers."))),
              fluidRow(
                column(width = 3,
                  box(width = NULL,
                      solidHeader = TRUE,
                      status = "primary",
                      "The chart indicates which companies had the highest number 
                      of taxi trips in 2023. Flash Cab ranks first, followed by 
                      Taxi Affiliation Services. They demonstrate significant 
                      dominance over other companies with over 1.2 million taxi 
                      trips. This suggests their strong market position and large 
                      customer base.")),
                column(
                  width = 9,
                  box(
                    title = "Top 10 companies with the highest number of trips",
                    solidHeader = TRUE,
                    status = "primary",
                    width = NULL,
                    shinycssloaders::withSpinner(
                    plotlyOutput("top_10_companies"),
                    type = getOption("spinner.type", default = 1),
                    color = getOption("spinner.color", default = "#0a87a6"),
                    size = getOption("spinner.size", default = 1)
                ))))),
      tabItem(tabName = "Lukasz",
              fluidRow(
                column(
                  width = 3,
                  box(
                    width = NULL,
                    title ='',
                    solidHeader = TRUE,
                    status = "primary",
                    dateRangeInput("date", "Select date range:",
                                     min = "2023-01-01", max = "2023-12-31",
                                     start = "2023-01-01", end = "2023-01-02",
                                     format = "dd-mm-yyyy"),
                  radioButtons("pick_drop", "The most popular:",
                               c("Pickup locations" = "pickups",
                                 "Dropoff locations" = "dropoffs"))),
                  box(
                    width = NULL,
                    solidHeader = TRUE,
                    status = "primary",
                    "Depending on the selection, the map shows the number of trips 
                    that started or ended in each district during the selected time 
                    period. We can observe that the parts of the city to or from 
                    which cabs go most often are the districts in the center and 
                    the district with the airport."
                  )),
                column(
                  width = 9,
                  box(
                    title = "Number of pickups / dropoffs in each district",
                    solidHeader = TRUE,
                    status = "primary",
                    width = NULL,
                    shinycssloaders::withSpinner(
                    leafletOutput("map", height = "550px"),
                    type = getOption("spinner.type", default = 1),
                    color = getOption("spinner.color", default = "#0a87a6"),
                    size = getOption("spinner.size", default = 1))
              )))))))

server <- function(input, output, session) { 
  
  #########
  # Wstęp #
  #########
  
  output$logo_mini <- renderImage({
    img_src <- "MiNIENG.svg"
    list(src = img_src, width = "60%", height = "auto")
  }, deleteFile = FALSE)
  
  ###############
  # część Marka #
  ###############
  
  # Smacznej herbatki lub kawusi dla czytającej/czytającego :3
  
  observe({
    unique_fuels <- unique(vehicles$Vehicle.Fuel.Source)
    unique_fuels <- unique_fuels[unique_fuels != "Electric"]
    unique_fuels[length(unique_fuels)+1] <- "All"
    updateSelectInput(session, "fuel", choices = unique_fuels)
  })
  
  ###############################
  # wheel chair accessible cars #
  ###############################
  
  output$wheelchairPlot <- renderPlotly({
    req(input$fuel)
    
    if(input$fuel != "All"){
      vehicles2 <- vehicles %>%
        filter(Vehicle.Fuel.Source == input$fuel)
    }else{
      vehicles2 <- vehicles
    }
    vehicles2 %>% 
      group_by(Vehicle.Model.Year) %>%
      summarise(
        Percent = sum(Wheelchair.Accessible == "Y") / length(Wheelchair.Accessible),
        Number = sum(Wheelchair.Accessible == "Y")
      ) %>%
      ungroup() %>% 
      filter(Vehicle.Model.Year != 2000) %>% 
      plot_ly(x = ~Vehicle.Model.Year, y = ~Number,
              type = "bar", color = I("#0a87a6")) %>% 
      layout(xaxis = list(title = "Vehicle Model Year"),
             yaxis = list(title = "Number of vehicles"))
  })
  
  ######################
  # vehicle model year #
  ######################
  
  output$modelYearPlot <- renderPlotly({
    req(input$fuel)
    
    if(input$fuel != "All"){
      
      vehicles2 <- vehicles %>%
        filter(!is.na(Vehicle.Model.Year),
               Vehicle.Fuel.Source == input$fuel)
    }else{
      vehicles2 <- vehicles %>%
        filter(!is.na(Vehicle.Model.Year))
    }
    
    zakres <- c(min(vehicles2$Vehicle.Model.Year), max(vehicles2$Vehicle.Model.Year))
    vehicles2 %>% 
      plot_ly(x = ~Vehicle.Model.Year, type = "violin", y0 = " ", color = I("#0a87a6")) %>%
      layout(xaxis = list(title = "Year", 
                          range = zakres,
                          nticks = range(zakres) - 1),
             yaxis = list(title = "Number of vehicles"))
  })
  
  ###############
  # Część Julki #
  ###############
  
  ################################
  # number of trips in each hour #
  ################################
  
  output$trips_per_hour <- renderPlotly({
    if (input$week_day == "All"){
      ramka <- tydzien %>% 
        select(hour, n)
    } else if (input$week_day == "Monday"){
      ramka <- tydzien %>% 
        select(hour, pon)
      colnames(ramka) <- c("hour", "n")
    } else if (input$week_day == "Tuesday"){
      ramka <- tydzien %>% 
        select(hour, wt)
      colnames(ramka) <- c("hour", "n")
    } else if (input$week_day == "Wednesday"){
      ramka <- tydzien %>% 
        select(hour, sr)
      colnames(ramka) <- c("hour", "n")
    } else if (input$week_day == "Thursday"){
      ramka <- tydzien %>% 
        select(hour, czw)
      colnames(ramka) <- c("hour", "n")
    } else if (input$week_day == "Friday"){
      ramka <- tydzien %>% 
        select(hour, pt)
      colnames(ramka) <- c("hour", "n")
    } else if (input$week_day == "Saturday"){
      ramka <- tydzien %>% 
        select(hour, sb)
      colnames(ramka) <- c("hour", "n")
    } else{
      ramka <- tydzien %>% 
        select(hour, ndz)
      colnames(ramka) <- c("hour", "n")
    }
    
    plot_ly(
      x = ~ramka$hour,
      y = ~ramka$n,
      type = "bar",
      color = I("#0a87a6"),
      hoverinfo = "text",
      textposition = "none",
      text = ~paste(
        "</br><b>Hour:</b> ",
        ramka$hour,
        "</br><b>Number of taxi trips:</b> ",
        ramka$n
      )) %>%
      layout(
        xaxis = list(title = list(text = "Hour")),
        yaxis = list(title = list(text = "Number of trips"))
      )
  })
  
  #####################################################
  # Top 10 companies with the highest number of trips #
  #####################################################
  
  output$top_10_companies <- renderPlotly({
    taxitrips %>% 
      group_by(Company) %>% 
      summarise(n = n()) %>% 
      top_n(10,n) %>% 
      plot_ly(x = ~n,
              y = ~fct_reorder(Company, n),
              color = I("#0a87a6"),
              type = "bar",
              hoverinfo = "text",
              textposition = "none",
              text = ~paste(
                "</br><b>Company:</b> ",
                Company,
                "</br><b>Number of trips:</b> ",
                n
              ))%>%
      layout(
        xaxis = list(title = 'Number of trips'),
        yaxis = list(title = list(text = 'Company name'))
      )
  })
  
  ##################
  # Tip percentage #
  ##################
  
  output$tip_percentage <- renderPlotly({
    tips_by_day <- tips_by_day %>% 
      filter(Fare!=0) %>% 
      mutate(tip_perc = Tips/Fare*100)
    
    tips_by_day %>% 
      group_by(weekday) %>% 
      summarise(mean = mean(tip_perc, na.rm = TRUE)) %>% 
      plot_ly(x = ~weekday, y = ~mean,
              type = "scatter",
              mode = "lines+markers",
              color = I("#0a87a6"),
              hovertemplate = ~paste0(
                "<extra></extra></br><b>%{x}</b>",
                "</br>Tip percentage: ",
                round(mean,3),
                "%"
              )) %>%
      layout(
        xaxis = list(title = list(text = 'Weekday'),
                     ticktext = list("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), 
                     tickvals = list(1,2,3,4,5,6,7),
                     tickmode = "array"),
        yaxis = list(title = list(text = 'Tip percentage'))
      )
  })
  
  #################
  # Część Łukasza #
  #################
  
  #######
  # Map #
  #######
  
  output$map <- renderLeaflet({
    
    taxitrips <- df_lukasz %>% 
      filter(date_without_hour >= as.Date(input$date[1], origin = "1970-01-01") & 
               date_without_hour <= as.Date(input$date[2], origin = "1970-01-01")) %>% 
      select(Pickup.Community.Area, Dropoff.Community.Area)
    if (input$pick_drop == "pickups") {
      z <- taxitrips %>% 
        group_by(Pickup.Community.Area) %>% 
        summarise(n = n())
      z$Pickup.Community.Area <- as.character(z$Pickup.Community.Area)
      chicago@data <- chicago@data %>%
        left_join(z, join_by("area_num_1" == "Pickup.Community.Area"))
    } else{
      z <- taxitrips %>% 
        group_by(Dropoff.Community.Area) %>% 
        summarise(n = n())
      z$Dropoff.Community.Area <- as.character(z$Dropoff.Community.Area)
      chicago@data <- chicago@data %>%
        left_join(z, join_by("area_num_1" == "Dropoff.Community.Area"))
    }
    
    bins <- c(0, 500, 1000, 5000, 10000, 20000, Inf)
    pal <- colorBin("Blues", domain = z$n, bins = bins)
    labels <-  sprintf("<strong>%s</strong><br/>Number of %s: <br/>%g",
                       chicago@data$community, input$pick_drop, chicago@data$n
                     ) %>% lapply(htmltools::HTML)
    
    map <- leaflet(chicago) %>% 
      addTiles() %>% 
      addPolygons(fillColor = ~pal(n),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>% 
      addLegend("bottomleft", 
                pal = pal,
                values = ~n, 
                title = paste0("Number of ", input$pick_drop), 
                labFormat = labelFormat(), 
                opacity = 0.7) 
    
    return(map)
    
  })
  }

shinyApp(ui, server)
