library(shiny)
library(data.table)
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(sf)
library(readr)
library(countrycode)
library(shinythemes)
library(plotly)


accidents_per_mio <- read.csv('accidents.csv')
gender_data <- read.csv('tran_sf_roadus_linear_2_0.csv')
gender_data <- gender_data%>%
  filter(geo != 'EU27_2020')%>%
  mutate(geo = countrycode(geo, "iso2c", "country.name"))
gender_data$geo[gender_data$geo == 'EL'] <- 'Greece'
gender_data$geo[gender_data$geo == 'UK'] <- 'United Kingdom'
gender_data$sex[gender_data$sex == 'M'] <- 'Male'
gender_data$sex[gender_data$sex == 'F'] <- 'Female'
gender_data$sex[gender_data$sex == 'T'] <- 'Total'
gender_data <- gender_data %>%
  mutate(age = case_when(
    age == "Y15-17" ~ "15-17",
    age == "Y18-24" ~ "18-24",
    age == "Y25-49" ~ "25-49",
    age == "Y50-64" ~ "50-64",
    age == "Y_GE65" ~ ">= 65",
    age == "Y_LT15" ~ "<= 15",
    TRUE ~ age  
  ))
gender_data <- gender_data %>%
  mutate(pers_cat = case_when(
    pers_cat == "DRIV" ~ "Driver",
    pers_cat == "PAS" ~ "Passenger",
    pers_cat == "PED" ~ "Pedestrian",
    TRUE ~ pers_cat  
  ))

gender_data <- gender_data %>%
  mutate(age = factor(age, levels = c("<= 15", "15-17", "18-24", "25-49", "50-64", ">= 65")))

vehicle_data <- read.csv('tran_sf_roadve__custom_11745501_linear_2_0.csv')

road_dead <- read.csv('dead_road.csv') %>%
  select(tra_infr, geo, TIME_PERIOD, OBS_VALUE) %>%
  filter(tra_infr %in% c('Motorways', 'Rural roads', 'Urban roads', 'Total'))

accidents_per_mio <- accidents_per_mio %>%
  filter(Country != 'EU-27') %>%
  mutate(Deaths = round(Deaths, digits = 2))

europe <- map_data("world") %>% 
  filter(region %in% c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", 
                       "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", 
                       "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", 
                       "Italy", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", 
                       "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", 
                       "Norway", "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", 
                       "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", 
                       "UK", "Vatican","Turkey")) %>%
  filter(long <= 35 & lat <= 70)

accidents_per_mio <- accidents_per_mio %>%
  mutate(Country = ifelse(Country == "United Kingdom", "UK", Country))

eu_merged <- left_join(europe, accidents_per_mio, by = c("region" = "Country"))

name_points <- eu_merged %>%
  group_by(region) %>%
  summarise(long = mean(long, na.rm = TRUE), lat = mean(lat, na.rm = TRUE), group = mean(group, na.rm = TRUE))

name_points[name_points$region == 'Switzerland',]$lat <- name_points[name_points$region == 'Switzerland',]$lat - 0.5
name_points[name_points$region == 'Liechtenstein',]$lat <- name_points[name_points$region == 'Liechtenstein',]$lat + 0.7
name_points[name_points$region == 'Liechtenstein',]$long <- name_points[name_points$region == 'Liechtenstein',]$long - 0.5
name_points[name_points$region == 'Luxembourg',]$lat <- name_points[name_points$region == 'Luxembourg',]$lat - 0.8
name_points[name_points$region == 'Luxembourg',]$long <- name_points[name_points$region == 'Luxembourg',]$long - 0.8

death_points <- name_points %>%
  mutate(long = long - 2, lat = lat - 2)

death_points <- left_join(death_points, accidents_per_mio, by = c('region' = 'Country'))

name_points_with_data <- name_points %>%
  filter(region %in% accidents_per_mio$Country)

ui <- navbarPage(
  theme = shinytheme("cosmo"),
  title = "Fatal road accidents in UE",
  tabPanel("Map",
           sidebarLayout(
             sidebarPanel(
               sliderInput("Rok1",
                           "Choose Year:",
                           min = 1999,
                           max = max(unique(accidents_per_mio$Rok)),
                           value = min(unique(accidents_per_mio$Rok)),
                           step = 1
               ),
               selectInput("selected_country1", "Choose country:", choices = sort(unique(accidents_per_mio$Country))),
               dataTableOutput("table")
             ),
             mainPanel(
               plotlyOutput("mapka", width = '100%', height = '600px'),
               plotlyOutput("DeathsPerYear", width = '100%')
             )
           )
  ),
  tabPanel("Road users",
           sidebarLayout(
             sidebarPanel(
               sliderInput("Rok2",
                           "Choose Year:",
                           min = 1999,
                           max = max(unique(accidents_per_mio$Rok)),
                           value = min(unique(accidents_per_mio$Rok)),
                           step = 1
               ),
               selectInput("selected_country2", "Choose country:", choices = sort(unique(accidents_per_mio$Country))),
               selectInput("selected_sex2", "Choose sex:", choices = unique(c('Male','Female','Total')))
             ),
             mainPanel(
               plotOutput("agesexPlot"),
               plotOutput("GenderType")
             )
           )
  ),
  tabPanel("Road type",
           sidebarLayout(
             sidebarPanel(
               sliderInput("Rok3",
                           "Choose Year:",
                           min = 1999,
                           max = max(unique(accidents_per_mio$Rok)),
                           value = min(unique(accidents_per_mio$Rok)),
                           step = 1
               ),
               selectInput("selected_country3", "Choose country:", choices = sort(unique(accidents_per_mio$Country)))
             ),
             mainPanel(
               plotOutput("roadTypePlot")
             )
           )
  )
)

server <- function(input, output) {
  
  output$mapka <- renderPlotly({
    mapa <- ggplot() +
      geom_polygon(data = europe, aes(x = long, y = lat, group = group), fill = "lightgrey", color = "grey") +
      geom_polygon(data = eu_merged[eu_merged$Rok == input$Rok1, ], 
                   aes(x = long, y = lat, group = group, fill = Deaths,
                       text = 
                                     paste("Country: ", region, "\nDeaths per million: ", round(Deaths, 2))
                   ), color = "darkgrey") +
      labs(title = paste("Deaths in car accidents per million people", input$Rok1)) +
      scale_fill_gradientn(colors = c('lightgreen', 'lightyellow', 'pink'), name = "Deaths", na.value = "gray90") +
      guides(fill = guide_colorbar(title = "Deaths", barwidth = 10, barheight = 1, ticks.colour = "black", ticks.linewidth = 1, title.position = "top", title.hjust = 0.5, label.position = "bottom")) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 20, family = "Arial"),
        legend.title = element_text(size = 16, family = "Arial"),
        panel.background = element_rect(fill = "lightblue", colour = "lightblue"),
        legend.position = "bottom",
        legend.direction = "horizontal"
      )
    
    ggplotly(mapa, tooltip = "text")
  })
  
  
  
  
  output$roadTypePlot <- renderPlot({
    data <- road_dead[road_dead$geo == input$selected_country3 & road_dead$TIME_PERIOD == input$Rok3, ]
    total <- sum(
      data%>%
        filter(tra_infr=='Total')%>%
        select(OBS_VALUE)
    )
    
    data <- data%>%filter(tra_infr!='Total')
    ggplot(data, aes(x = tra_infr, y = OBS_VALUE/total*100, fill = tra_infr)) +
      geom_col(fill = 'darkgrey') +
      labs(x = "Type of Road", y = "Percentage", title = "Road Infrastructure Death Rates") +
      theme(legend.position = 'None',
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "white", colour = "white"),
            plot.background = element_rect(fill = "white", colour = "white"),
            plot.title = element_text(size = 20, family = "Arial"),
            axis.title.x = element_text(size = 15, family = "Arial"),
            axis.title.y = element_text(size = 15, family = "Arial"),
            axis.text = element_text(size = 15, family = "Arial")
            )
  })
  
  output$agesexPlot <- renderPlot({
    data <- gender_data[gender_data$geo == input$selected_country2 & gender_data$TIME_PERIOD == input$Rok2, ]
    data <- na.omit(data)
    data <- data%>%
      filter(!(age %in% c('TOTAL','UNK')))%>%
      filter(sex !='UNK')
    
   
    total <- sum(data%>%
      filter(sex == input$selected_sex2)%>%
      select(OBS_VALUE)
    )
    data <- data%>%
      filter(sex == input$selected_sex2)
    if(input$selected_sex2 == 'Male'){
      color <- "#619CFF"
    }else if(input$selected_sex2 == 'Female'){
      color <- "#F8766D"
    }else{
      color <- 'darkgrey'
    }
    
    ggplot(data, aes(x = data$age, y = OBS_VALUE / total * 100)) +
      geom_col(position = "dodge",fill = color) +
      labs(x = "Age", y = "Percentage", title = "Percentage of people involved in car accidents by age and sex") +
      theme(legend.position = 'right',
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "white", colour = "white"),
            plot.title = element_text(size = 20, family = "Arial"),
            axis.title.x = element_text(size = 15, family = "Arial"),
            axis.title.y = element_text(size = 15, family = "Arial"),
            axis.text = element_text(size = 15, family = "Arial")
            )
  })
  
  output$table <- renderDataTable({
    accidents_per_mio[accidents_per_mio$Rok == input$Rok1, ] %>% select(Country, Deaths)
  }, options = list(scrollX = TRUE, scrollY = "200px", scrollCollapse = TRUE, lengthChange = FALSE))
  
  output$DeathsPerYear <- renderPlotly({
    data <- accidents_per_mio %>%
      filter(Country == input$selected_country1)
    
    p <- ggplot(data, aes(x = Rok, y = Deaths, text = paste0("Year: ", Rok, "<br>Deaths: ", Deaths))) +
      geom_line() +
      geom_point() +
      labs(x = "Year", y = "Deaths per million people", title = paste("Deaths per million people in", input$selected_country1)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, family = "Arial"),
        axis.title.x = element_text(size = 15, family = "Arial"),
        axis.title.y = element_text(size = 15, family = "Arial"),
        axis.text = element_text(size = 15, family = "Arial")
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$GenderType <- renderPlot({
    data <- gender_data %>% 
      filter(TIME_PERIOD == input$Rok2,geo == input$selected_country2,!(pers_cat%in% c('TOTAL','UNK')),!(sex%in% c('Total','UNK') ))
    
    ggplot(data, aes(x = pers_cat, y = OBS_VALUE, fill = sex))+
      geom_col(position = 'dodge')+
      labs(x = "", y = "Deaths per milion people", title = "Deaths by road user" ) +
      scale_fill_manual(values = c("Female" = "#F8766D", "Male" = "#619CFF")) +
      theme(legend.position = 'right',
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "white", colour = "white"),
            plot.title = element_text(size = 20, family = "Arial"),
            axis.title.x = element_text(size = 15, family = "Arial"),
            axis.title.y = element_text(size = 15, family = "Arial"),
            axis.text = element_text(size = 15, family = "Arial"),
            legend.text = element_text(size = 15, family = "Arial"),
            legend.title = element_text(size = 15, family = "Arial")
            )
  })
}

shinyApp(ui = ui, server = server)
