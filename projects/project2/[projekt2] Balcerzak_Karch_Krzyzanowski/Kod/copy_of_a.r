library(shiny)
library(tidyr)
library(dplyr)
library(plotly)
library(bslib)
library(ggplot2)
library(shinyWidgets)
library(wesanderson)
library(leaflet)
library(rnaturalearth)
library(rworldmap)
library(shinydashboard)
library(eurostat)
library(htmltools)
library(RColorBrewer)

eu1 <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Republic of Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovak Republic", "Slovenia", "Spain", "Sweden")
data <- read.csv('MTN.csv')
data1 <- read.csv('INV.csv')
data <- bind_rows(data, data1)
data <- data[data$MEASURE == "EUR", ]
pass <- read.csv('PASSENGER_TRANSPORT.csv')
pass <- pass[pass$Variable != "Total inland passenger transport",]
data <- data[data$Country %in% eu1, ]
data1 <- data1[data$Country %in% eu1, ]
pass <- pass[pass$Country %in% eu1, ]





# mapka -------------------------------------------------------------------


#train_stations <- read.csv("train_stations_europe.csv")
europeanUnion <-
  c(
    "Austria",
    "Belgium",
    "Bulgaria",
    "Croatia",
    "Cyprus",
    "Czechia",
    "Denmark",
    "Estonia",
    "Finland",
    "France",
    "Germany",
    "Greece",
    "Hungary",
    "Ireland",
    "Italy",
    "Latvia",
    "Lithuania",
    "Luxembourg",
    "Malta",
    "Netherlands",
    "Poland",
    "Portugal",
    "Romania",
    "Slovakia",
    "Slovenia",
    "Spain",
    "Sweden"
  )
e_pop <-
  c(
    9104772,
    11754004,
    6447710,
    3850894,
    920701,
    10827529,
    5932654,
    1373101,
    5563970,
    68070697,
    84358845,
    10394055,
    9597085,
    5194336,
    58850717,
    1883008,
    2857279,
    660809,
    542051,
    17947406,
    36753736,
    10467366,
    19051562,
    5428792,
    2116792,
    48059777,
    10521566
  )

eu <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin %in% europeanUnion)
passengers <- read.csv("passengers.csv", sep = ";")
colnames(passengers)[1] <- "Country"
passengers[passengers == 0] <- NA
passengers <- passengers %>% arrange(Country)
passengers[, 2:20] <- floor(passengers[, 2:20] / e_pop * 100000*100)/100
eu2 <- eu %>% left_join(passengers, join_by(admin == Country))


eu_air <- get_eurostat("avia_paocc", select_time = "A")

eu_air_redone <-
  eu_air %>% mutate(yr = substring(TIME_PERIOD, 1, 4)) %>%
  filter(yr %in% 2004:2022) %>%
  mutate(yr = paste0("X", yr)) %>%
  group_by(yr, geo) %>%
  summarise(val = sum(values)) %>%
  pivot_wider(names_from = yr, values_from = val) %>%
  right_join(data.frame(eu) %>% select(admin, iso_a2_eh),
             join_by(geo == iso_a2_eh)) %>%
  arrange(admin) %>%
  select(1:20)
eu_air_redone[, 2:20] <- floor(100*eu_air_redone[, 2:20] / e_pop)/100

eu2air <- eu %>% left_join(eu_air_redone, join_by(iso_a2_eh == geo))


eu_road <- get_eurostat("road_if_motorwa", select_time = "A")

eu_road_redone <-
  eu_road %>% mutate(yr = substring(TIME_PERIOD, 1, 4)) %>%
  filter(yr %in% 2004:2022) %>%
  mutate(yr = paste0("X", yr)) %>%
  group_by(yr, geo) %>%
  summarise(val = sum(values)) %>%
  pivot_wider(names_from = yr, values_from = val)
eu2road <-
  eu %>% left_join(eu_road_redone, join_by(iso_a2_eh == geo))


# co2 ---------------------------------------------------------------------

specific_co2_emissions_per_tonne_2 <- read.csv("specific-co2-emissions-per-tonne-2.csv")

names(specific_co2_emissions_per_tonne_2) <- c("Year.text", "Inland transport", "Maritime transport", "Rail transport", "Road transport")

specific_co2_emissions_per_tonne_2 <- specific_co2_emissions_per_tonne_2 %>% 
  gather(Category, Emissions, -Year.text)

greenhouse_gas_emissions_from_transport_7 <- read.csv("greenhouse-gas-emissions-from-transport-7.csv")

greenhouse <- greenhouse_gas_emissions_from_transport_7 %>% 
  select(Year.year, International.aviation.number, International.maritime.transport.number, Domestic.Aviation.number, Road.transport.number, Domestic.navigation.number, Railways.number) %>% 
  rename("Year" = Year.year, 
         "International aviation" = International.aviation.number, 
         "International maritime transport" = International.maritime.transport.number, 
         "Domestic aviation" = Domestic.Aviation.number, 
         "Road transport" = Road.transport.number, 
         "Domestic navigation" = Domestic.navigation.number, 
         "Railways" = Railways.number) %>% 
  filter(Year <= 2021) %>% 
  gather(Category, Change, -Year) 

co_emissions_by_sector <- read.csv("co-emissions-by-sector.csv")
emissions <- co_emissions_by_sector %>% 
  select(Entity, Year, Carbon.dioxide.emissions.from.transport) %>% 
  filter(Entity %in% c("Africa", "Antarctica", "Asia", "Europe", "North America", "Australia", "South America"))
emissions2 <- emissions %>% 
  group_by(Entity) %>% 
  summarise(Mean = round(mean(Carbon.dioxide.emissions.from.transport) / 1000000)) %>% 
  arrange(-Mean)


# app ---------------------------------------------------------------------


ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Europe Transport"),
  dashboardSidebar(sidebarMenu(
    menuItem("CO2 Emissions", tabName = "co2", icon = icon("bars-staggered")),
    menuItem("Expenses", tabName = "rails", icon = icon("dollar-sign")),
    menuItem("Map", tabName = "map", icon = icon("map"))
  )),
  dashboardBody(tabItems(
    tabItem(tabName = "co2",
            fluidRow(
              box(
                title = "Barplot", background = "maroon", solidHeader = F,
                width=8, plotlyOutput("Co2emissions")
              ),
              box(
                title = "Europe CO2 Emission", background = "aqua", solidHeader = F,
                width=4, checkboxGroupInput("checkbox1",
                                            "Choose form of transport:",
                                            unique(specific_co2_emissions_per_tonne_2$Category),
                                            selected = "Inland transport"),
                        sliderInput("range1",
                                    "Choose years:",
                                    value = c(min(specific_co2_emissions_per_tonne_2$Year.text), max(specific_co2_emissions_per_tonne_2$Year.text)),
                                    min = min(specific_co2_emissions_per_tonne_2$Year.text),
                                    max = max(specific_co2_emissions_per_tonne_2$Year.text),
                                    step = 1),
                p("This plot shows the emissions of CO2 in recent years, grouped by the form of transport. What we can deduce is rail transport has had the lowest emissions ever since it was categorized. Moreover, there is a slight downward trend in the inland transport emissions, making the future of avoiding huge migration crisis almost plausible.")
              )
            ),
            fluidRow(
              box(
                title = "Changes in greenhouse gas emission over the years", background = "aqua", solidHeader = F,
                width=4, checkboxGroupInput("checkbox2",
                                            "Choose form of transport:",
                                            unique(greenhouse$Category),
                                            selected = "Railways"),
                sliderInput("range2",
                            "Choose years:",
                            value = max(greenhouse$Year),
                            min = min(greenhouse$Year),
                            max = max(greenhouse$Year),
                            step = 1),
                p("This graph shows the trends in greenhouse gas emissions by different modes of transport. After early 2000s, the emissions in all sectors but Aviation were kept at constant levels or, in some cases, lowered. What is noteworthy is the drastic plummet of all emissions in 2020 due to some peculiar, undisclosed circumstances.")
              ),
              box(
                title = "Lines", background = "maroon", solidHeader = F,
                width=8, plotlyOutput("co2changes")
              )
            ),
            fluidRow(
              box(
                title = "Barplot", background = "maroon", solidHeader = F,
                width=8, plotlyOutput("continents")
              ),
              box(
                title = "CO2 emissions by continents", background = "aqua", solidHeader = F,
                width=4, p("This humble plot compares the emissions of Co2 in different continents, but do not underestimate it, as this graph is a great counter-example to the thesis \"Lowering CO2 emissions in Europe won't help a bit\". While our impact on the environment is - thankfully - not the most severe, we still produce a significant portion of carbon dioxide.")
              )
            )
    ),
    tabItem(tabName = "rails",
            fluidRow(
              box(
                title = "Barplot", background = "aqua", solidHeader=F,
                width=4,
                  selectInput("kraj", "Choose countries:", unique(data$Country), multiple = TRUE, selected = "France"),
                  selectInput("typ", "Choose type of expenses:", unique(data$Variable)),
                  numericRangeInput(inputId = "zakres", label = "Choose range of years:", value = c(2000, 2021)),
                  radioButtons("railorroad", "Choose type of passenger transport:", c("Road" = "Road", "Rail" = "Rail"), inline = TRUE)
              ),
              box(
                title="Barplot", background = "maroon", solidHeader = F,
                width=8, plotOutput("wykres1")
              )
            ),
            fluidRow(
              box(
                title = "Compact Analysis of Passengers", background = "aqua", solidHeader = TRUE,
                width=4, p("These plots aim to compare the passengers by different transport types to other variables, in order to see, whether they are correlated. Having said that, we must keep in mind that correlation does not equal causation. When we suspect that two variables are correlated, we should assume that either one can be caused by the other, or that they are influenced by a different, hidden variable.")
              ),
              box(
                title="Barplot", background = "maroon", solidHeader = TRUE,
                width=8, plotOutput("wykres2")
              ),
            )
    ),
    tabItem(tabName = "map",
            fluidRow(
              column(width=12, height=50,
                     leafletOutput("map",  width = "100%", height = 620)
              ),
            ),
            fluidRow(
              box(width=6, background = "aqua",
                  sliderInput(
                    "year",
                    "Pick desired year:",
                    min = 2004,
                    max = 2022,
                    value = 2010
                  ),
                  selectInput(
                    inputId = "frame",
                    label = "Pick desired information branch:",
                    choices = c(
                      "Aviation" = "eu2air",
                      "Railway" = "eu2",
                      "Motorway" = "eu2road"
                    )
                  )
              ),
              box(width=6, title="Intuitive Map of Europe", background = "aqua",
                  p("Why look at the dull as ditchwater graphs to grasp some information about the passengers in EU, when you can gaze upon this neat, interactive, user-friendly map! It shows the amount of passengers in the chosen year in countries (per 100k people) or - in case of Motorways - total length of motorways in each country. This map offers some intriguing results. For instance, during the undisclosed peculiar event in 2020, Aviation in Europe got absolutely obliterated, while railway transport \"only\" suffered less than 50% decrease in amount of passengers. Length of Motorways, unsurprisingly, remained almost unchanged.")
                  )
            )
    )
  ),
  setBackgroundImage(
    src = "https://www.easyhaul.com/blog/wp-content/uploads/2021/07/Blog-EasyHaul-Modes-of-Transport-Title-1.png",
   #src = "https://d2lv662meabn0u.cloudfront.net/cartoonito/dynamic/character/00000000/22/5eb9dd6c-bf7f-44da-865b-5443c73665d4_1619544232.jpg",
    shinydashboard = TRUE
  ))
)

server <- function(input, output) {
  output$Co2emissions <- renderPlotly({
      co2_emissions<-specific_co2_emissions_per_tonne_2 %>% 
        filter(Year.text>=input$range1[1], Year.text<=input$range1[2],
               Category %in% input$checkbox1)
      plot_ly(
        co2_emissions,
        x = ~Year.text,
        y = ~Emissions,
        color = ~Category,
        colors = "Set1",
        type = "bar",
        text = paste0("<b>Emissions: </b>", co2_emissions$Emissions,
                      "<br><b>Year: </b>", co2_emissions$Year.text,
                      "<br><b>Category: </b>", co2_emissions$Category),
        hoverinfo = "text") %>% 
      layout(
        title = "<b>Europe CO2 emissions per tonne-km</b>",
        xaxis = list(title = "Year", 
                     type = "category", 
                     categoryorder = "array", 
                     categoryarray = unique(co2_emissions$Year.text)),
        yaxis = list(title = "CO2 emissions (g/tkm)"),
        legend = list(title = "Form of transport"),
        margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4)
      )
  })
  
  output$co2changes <- renderPlotly({
    
    green<-greenhouse %>% 
      filter(Year<=input$range2[1],
             Category %in% input$checkbox2)
      plot_ly(
        green,
        x = ~Year,
        y = ~Change,
        color = ~Category,
        colors = "Set2",
        type = "scatter",
        mode = "lines",
        text = paste0("<b>Change: </b>", green$Change,
                      "<br><b>Year: </b>", green$Year,
                      "<br><b>Category: </b>", green$Category),
        hoverinfo = "text") %>% 
      layout(
        title = "<b>Change over the years of greenhouse gas emissions in the EU</b>",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Change from 1990"),
        legend = list(title = "Form of transport"),
        margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4)
      )
  })
  
  output$continents <- renderPlotly({
    emissions2 %>% 
      plot_ly(
        x = ~Mean,
        y = ~Entity,
        marker = list(color = "turquoise3"),
        type = "bar",
        text = paste0(emissions2$Mean),
        hoverinfo = "text") %>% 
      layout(
        title = "<b>CO2 emissions from transport by continents</b>",
        xaxis = list(title = "CO2 emissions in millions of tonnes"),
        yaxis = list(title = "", categoryorder = "total ascending"),
        margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4)
      )
  })
  
  output$wykres1 <- renderPlot({
    ggplot(data[data$Variable == input$typ & data$Country %in% c(input$kraj) & data$Year %in% c(c(input$zakres)[1]:c(input$zakres)[2]), ], 
           aes(fill = Country, x = as.factor(Year), y = Value / 1000000000)) + 
      geom_bar(position = "dodge", stat = "identity") + scale_y_continuous(expand = c(0, 0)) + theme_minimal() +
      scale_fill_brewer(palette="Set1")+ theme(legend.position = "bottom") +
      labs(x = "Year", y = "Value (billions of EUR)", title = paste(input$typ, "expenses in", paste0(c(input$kraj), collapse = ", "))) + 
      theme(plot.title = element_text(size = 16))
  })
  
  output$wykres2 <- renderPlot({
    railorroad <- switch(input$railorroad, Road = "Road passenger transport", Rail = "Rail passenger transport")
    ggplot(pass[pass$Variable == railorroad & pass$Country %in% c(input$kraj) & pass$Year %in% c(c(input$zakres)[1]:c(input$zakres)[2]), ], 
           aes(fill = Country, x = as.factor(Year), y = Value / 1000)) + 
      geom_bar(position = "dodge", stat = "identity") + scale_y_continuous(expand = c(0, 0)) + theme_minimal() + 
      scale_fill_brewer(palette="Set1") + theme(legend.position = "none", plot.title = element_text(size = 16)) +
      labs(x = "Year", y = "Value (billions km per passenger)", title = paste(railorroad, "in", paste0(c(input$kraj), collapse = ", ")))
  })

  
  output$map <- renderLeaflet({
    yr <- paste0("X", input$year)
    leaflet(get(input$frame)) %>%
      setView(16, 47.8, 3) %>%
      addTiles() %>%
      addPolygons(
        label = ~ lapply(
          paste0(
            admin,
            "<br>",
            ifelse(
              input$frame == "eu2road",
              "Length of Motorways (KM): ",
              "Passengers (per 100k population): "
            ),
            select(get(input$frame), contains(yr))[[1]],
            "<br>",
            "Year: ",
            substring(yr, 2, 5)
          ),
          htmltools::HTML
        ),
        labelOptions = labelOptions(textsize = "12px"),
        stroke = FALSE,
        fillOpacity = 0.5,
        smoothFactor = 0.5,
        fillColor = ~ colorQuantile(
          n = 5,
          case_when(
            input$frame == "eu2" ~ "YlGnBu",
            input$frame == "eu2road" ~
              "PuRd",
            input$frame == "eu2air" ~
              "YlOrRd"
          ),
          select(get(input$frame), contains(yr))[[1]]
        )(select(get(input$frame), contains(yr))[[1]])
      ) %>%
      addLegend(
        "bottomright",
        colors = case_when(
          input$frame == "eu2" ~ brewer.pal(5, name = "YlGnBu"),
          input$frame == "eu2road" ~ brewer.pal(5, name = "PuRd"),
          input$frame == "eu2air" ~ brewer.pal(5, name = "YlOrRd")
        ),
        labels = paste0("[",floor(100*quantile(
          select(get(input$frame), contains(yr))[[1]],
          c(0, 0.2, 0.4, 0.6, 0.8), na.rm = TRUE))/100,"; ", floor(100*quantile(
          select(get(input$frame), contains(yr))[[1]],
          c(0.2, 0.4, 0.6, 0.8, 1),
          na.rm = TRUE
        ))/100, ")"),
        title = ifelse(
          input$frame == "eu2road",
          "Length of Motorways (KM)",
          "Passengers (per 100k population)"
        ),
        opacity = 1
      )
    
  })

}

shinyApp(ui, server)