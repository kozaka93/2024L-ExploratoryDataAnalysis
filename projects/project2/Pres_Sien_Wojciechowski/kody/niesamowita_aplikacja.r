library(shiny)
library(plotly)
library(dplyr)
library(rvest)
library(forcats)
library(ggplot2)
library(bslib)
library(gganimate)
library(gapminder)
library(maps)
library(mapdata)
library(gridExtra)
library(shinythemes)
library(readr)

# PREPARING DATA

port <- read.csv("data/World_Port_Index.csv")
file <- read_csv("data/file.csv")

data1 <- read.csv("data/ShipData1.csv")
data2 <- read.csv("data/ShipData2.csv")
data <- rbind(data1, data2)
# --------------------------
# HARVOURS - PREPARING DATA

url <- "https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2"

page <- read_html(url)

df_codes <- html_element(page, "table.wikitable.sortable") %>% 
  html_table() %>% 
  select("Code", "Country name (using title case)") %>% 
  rename("COUNTRY" = "Code",
         "COUNTRY_NAME" = "Country name (using title case)")

df = merge(port, df_codes, by = "COUNTRY")

df$COUNTRY_NAME[df$COUNTRY == "GB"] <- "Great Britain"
df$COUNTRY_NAME[df$COUNTRY == "RU"] <- "Russia"
df$COUNTRY_NAME[df$COUNTRY == "NL"] <- "Netherlands"
df$HARBORSIZE[df$HARBORSIZE == " "] <- "V"
df$HARBORSIZE[df$HARBORSIZE == "V"] <- "Very small"
df$HARBORSIZE[df$HARBORSIZE == "S"] <- "Small"
df$HARBORSIZE[df$HARBORSIZE == "L"] <- "Large"
df$HARBORSIZE[df$HARBORSIZE == "M"] <- "Medium"

# ---------------------------------
# PIRACY - PREPARING DATA

file$lat<-gsub( " .*$", "",file$position)
file$lon<-substr(file$position,nchar(file$lat)+2,nchar(file$position))

f <- function(dms_string) {
  parts <- unlist(strsplit(dms_string, "[°'\"E]"))
  degrees <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  seconds <- as.numeric(parts[3])
  decimal_degrees <- degrees + minutes / 60 + seconds / 3600
  return(decimal_degrees)
}

file$lat<-lapply(file$lat, f)
file$lon<-lapply(file$lon,f)
file$lat<-as.numeric(file$lat)
file$lon<-as.numeric(file$lon)
file$year<-as.numeric(substr(file$date,1,4))
file$n<-case_when(
  file$navArea == 'I' ~ 'UK',
  file$navArea == 'II' ~ 'France',
  file$navArea == 'III' ~ 'Spain',
  file$navArea == 'IV' ~ 'USA',
  file$navArea == 'V' ~ 'Brazil',
  file$navArea == 'VI' ~ 'Argentina',
  file$navArea == 'VII' ~ 'South Africa',
  file$navArea == 'VIII' ~ 'India',
  file$navArea == 'IX' ~ 'Pakistan',
  file$navArea == 'X' ~ 'Australia',
  file$navArea == 'XI' ~ 'Japan',
  file$navArea == 'XII' ~ 'USA',
  file$navArea == 'XIII' ~ 'Russia',
  file$navArea == 'XIV' ~ 'New Zealand',
  file$navArea == 'XV' ~ 'Chile',
  file$navArea == 'XVI' ~ 'Peru'
)
file$month<-substr(file$date,6,7)
file$monthday<-substr(file$date,6,10)
days<-c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
file$day<-days[as.POSIXlt(file$date)$wday + 1]
temp<-{file %>%
    group_by(month,year) %>% 
    summarise(n=n()) %>% 
    group_by(month) %>% 
    mutate(mean=mean(n)) %>% 
    select(mean)} %>% 
  unique()
m<-as.data.frame(temp)

file$hostility=toupper(file$hostility)
file$category<-case_when(
  grepl('ROB', file$hostility) ~ 'robbery',
  grepl('PIRATE', file$hostility) ~ 'piracy',
  grepl('PIRTATES', file$hostility) ~ 'piracy',
  grepl('PRIATES', file$hostility) ~ 'piracy',
  grepl('PRATES', file$hostility) ~ 'piracy',
  grepl('PIERATES', file$hostility) ~ 'piracy',
  grepl('TERROR', file$hostility) ~ 'acts of terrorism',
  grepl('MILITANT', file$hostility) ~ 'acts of terrorism',
  grepl('MILITARY', file$hostility) ~ 'military-related hostility',
  grepl('NAVY', file$hostility) ~ 'military-related hostility',
  grepl('LTTE', file$hostility) ~ 'military-related hostility',
  grepl('MILITIAMEN', file$hostility) ~ 'military-related hostility',
  grepl('NAVAL', file$hostility) ~ 'military-related hostility',
  grepl('SUSP', file$hostility) ~ 'miscellaneous hostility',
  grepl('UNAUTHOR', file$hostility) ~ 'miscellaneous hostility',
  .default = 'uncathegorized acts'
)

#___________________________________________

# UI
ui0 <- fluidPage(
  titlePanel(h1("MARITIME TRANSPORT AND PIRACY", align = "center")),
  fluidRow(
    paste("Our objective was to make an interactive site about maritime transport, with focus on harbours and piracy.","Maritime transport is an important mode of transport, accounting for approximately 80% of international trade in 2020. Explore our findings on the topic by choosing a desired tab.",sep="\n"),
    style = 'margin-left: 10%; margin-right: 10%;',
    
    
  )
)

ui1 <- fluidPage(
  titlePanel("Harbours all over the world"),
  fluidRow(
    column(12,
           p("This section is devoted to harbours all over the world. In the checkbox below you can choose the sizes of harbours you want to be taken into account. There are also buttons on the right, click on one of them to change the plot shown underneath. You can either choose a global map of harbours or a bar plot showing countries with the highest numbers of ports of the chosen size."),
           style = "margin-top: 20px; margin-bottom: 20px;"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("checkboxes", "Choose harbour sizes to display:", 
                         choices = list("Very small" = "Very small", 
                                        "Small" = "Small", 
                                        "Medium" = "Medium",
                                        "Large" = "Large"),
                         selected = unique(df$HARBORSIZE))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map",
                 fluidRow(
                   column(8, plotlyOutput("geoPlot")),
                   column(10, 
                          h3("Map Information"),
                          p("This map shows ports around the world. You can see details about each port by hovering over the dots. You can also zoom the map in and out and turn the globe."),
                          style = "margin-top: 20px;"
                   )
                 )
        ),
        tabPanel("Bar Plot",
                 fluidRow(
                   column(8, plotlyOutput("barPlot")),
                   column(10, 
                          h3("Bar Plot Information"),
                          p("This bar plot displays the top 10 countries with the highest number of harbours of the selected sizes. The number of harbours for each country is shown on the y-axis, while the countries are listed on the x-axis."),
                          style = "margin-top: 20px;"
                   )
                 )
        )
      )
    )
  )
)

ui2 <- fluidPage(
  titlePanel("US Ocean Traffic in 2020"),
  fluidRow(
    column(12,
           p("This section is devoted to ocean traffic in the US in 2020, which is the year of the start of the COVID-19 pandemic. The map below shows the density of average daily ship loggings on the waters of the United States during a selected period."),
           style = "margin-top: 20px; margin-bottom: 20px;"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "day",
        label = "Day:",
        min = as.Date("2020-01-01"),
        max = as.Date("2020-12-31"),
        value = c(as.Date("2020-04-10"), as.Date("2020-07-18")),
        timeFormat = "%Y-%m-%d"
      ),
      width = 3
    ),
    mainPanel(
      #shiny::markdown("The map shows the density of average daily ship loggings on the waters of the United States during a selected period."),
      plotlyOutput("density_map", height = "500px"),  # Adjust height for a larger plot
      width = 9
    )
  )
)

ui3 <- fluidPage(
  titlePanel("MARITIME TRANSPORT AND PIRACY"),
  
  fluidRow(
    column(12,
           p("This section is devoted to piracy acts and hostile actions. Seaborne piracy against transport vessels is a significant issue to this day. Below you can examine the statistical aspects of piracy acts and hostile actions against commercial shipping."),
           p("Use the dropdown below to select the type of hostility. There are various plots available, including a map, bar plot, and two distribution charts. You can toggle between different views using the provided checkboxes."),
           style = "margin-top: 20px; margin-bottom: 20px;"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        'host',
        'Select type of hostility',
        selected = 'all',
        multiple = FALSE,
        choices = c('all', unique(file$category))
      ),
      checkboxInput("type", "Do you want to change to violin plot?", FALSE),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map",
                 fluidRow(
                   column(10, plotlyOutput("mapPlot_p", width = '100%', height = '100%')),
                   column(10, 
                          h3("Map Information"),
                          p("This map shows different types of hostility incidents throughout years. You can see details about each incident by hovering over the dots. As you may observe on the map our data is focused on the east. The most attacked locations include the Gulf of Guinea, the Horn of Africa and the Strait of Malacca."),
                          style = "margin-top: 20px;"
                   )
                 )
        ),
        tabPanel("Bar Plot",
                 fluidRow(
                   column(11, plotlyOutput("barPlot_p")),
                   column(11, 
                          h3("Bar Plot Information"),
                          p("This bar plot displays the number of hostility throughout years. The low values for older years are mostly based on lack of data from this time. Nonetheless, we can observe that, for all types of hostilities, since the peak in 2010 the number of incidents has a declining trend."),
                          style = "margin-top: 20px;"
                   )
                 )
        ),
        tabPanel("Month Plot",
                 fluidRow(
                   column(11, plotlyOutput("violinPlot_p")),
                   column(11, 
                          h3("Month Plot Information"),
                          p("This plot shows the distribution of hostility incidents throughout months. For all types of hostilities the highest median can be seen in March and November, while the lowest in September."),
                          style = "margin-top: 20px;"
                   )
                 )
        ),
        tabPanel("Day Plot",
                 fluidRow(
                   column(11, plotlyOutput("dayPlot_p")),
                   column(11, 
                          h3("Day Plot Information"),
                          p("This plot shows the distribution of hostility incidents throughout days of the week. For all types of hostilities the highest median can be observed on Tuesday and the lowest on Sunday, with the probable cause for the latest being the lower level of transport on Sundays."),
                          style = "margin-top: 20px;"
                   )
                 )
        )
      )
    )
  )
  
  # fluidRow(
  #   column(11,
  #          textOutput("text")
  #   )
  # )
)


#___________________________________________


server <- function(input, output, session) {
  #Global Harbours
  filteredData <- reactive({
    df[df$HARBORSIZE %in% input$checkboxes, ]
  })
  
  output$geoPlot <- renderPlotly({
    plot_geo(filteredData(),
             lat = ~LATITUDE,
             lon = ~LONGITUDE,
             text = ~paste0("Port name: ", PORT_NAME, "<br>Country: ", COUNTRY_NAME, "<br>Country code: ", COUNTRY),
             hoverinfo = 'text') %>%
      add_markers() %>% 
      layout(
        title = "Global Ports",
        geo = list(
          showland = TRUE,
          landcolor = "rgb(243, 243, 243)",
          subunitwidth = 1,
          countrywidth = 1,
          subunitcolor = "rgb(217, 217, 217)",
          countrycolor = "rgb(217, 217, 217)",
          coastlinecolor = 'rgb(0, 0, 0)',
          projection = list(type = 'orthographic'),
          showcountries = TRUE
        ),
        margin = list(l = 0, r = 0, t = 80, b = 0)
      )
  })
  
  output$barPlot <- renderPlotly({
    harborCounts <- filteredData() %>%
      group_by(COUNTRY_NAME) %>% 
      summarise(n = n()) %>% 
      arrange(desc(n)) %>% 
      head(10)
    
    plot_ly(harborCounts, x = ~fct_relevel(COUNTRY_NAME, COUNTRY_NAME), y = ~n, type = 'bar') %>%
      layout(
        title = "Top 10 countries with the highest number of harbours",
        subtitle = "of a chosen size",
        xaxis = list(title = "Country"),
        yaxis = list(title = "Number of harbours"),
        margin = list(l = 50, r = 0, t = 80, b = 0)
      )
  })
  
  #US Ocean Traffic
  output$density_map <- renderPlotly({
    filtered_data <- data %>%
      filter(
        BaseDateTime >= as.Date(input$day[1], origin = "2020-01-01") &
          BaseDateTime <= as.Date(input$day[2], origin = "2020-01-01")
      )
    
    plot_ly(
      data = filtered_data,
      type = 'densitymapbox',
      lat = ~ LAT,
      lon = ~ LON,
      coloraxis = 'coloraxis',
      radius = 1
    ) %>%
      layout(
        title = "Density of transport ships on the US waters in 2020",
        mapbox = list(
          style = "open-street-map",
          center = list(lon = -100, lat = 40),
          zoom = 2
        ),
        margin = list(l = 0, r = 0, t = 45, b = 0),
        coloraxis = list(colorscale = "Viridis"),
        height = 500
      )
  })


# PIRACY ACTS

  
  filtered_data <- reactive({
    data <- file
    if (input$host != 'all') {
      data <- data[data$category %in% input$host, ]
    }
    data
  })
  
  output$mapPlot_p <- renderPlotly({
    # 1) making the animated map
    
    w1 <- map_data("world") %>%
      filter(region != "Antarctica")
    w1 <- as.data.frame(w1)
    world <- ggplot() +
      geom_polygon(
        data = w1,
        aes(x = long, y = lat, group = group),
        color = 'grey10',
        linewidth = 0.35,
        fill = 'grey21'
      )
    
    g <-
      world + geom_point(
        data = filtered_data(),
        aes(
          x = lon,
          y = lat,
          frame = year,
          text = sprintf(
            "NAVAREA: %s<br>coordinator: %s<br>subregion: %s",
            navArea,
            n,
            subreg
          )
        ),
        color = 'mediumblue'
      ) +
      enter_fade() +
      exit_fade()
    #scale_color_manual(values=c("mediumblue","#ca009e", "#ff0069","#ff6941","#ffb83a","#a0b33e"))
    
    g<-ggplotly(g, tooltip = "text")
    g %>%
      animation_opts(
        400,easing=NULL, transition =0
      ) %>% 
      layout(
        updatemenus = list(
          list(type = "buttons", x = 0, y = -0.19, xanchor='left', yanchor='bottom',
               buttons = list(
                 list(method = "animate",
                      args = list(NULL, list(fromcurrent = TRUE, mode = "immediate")), 
                      label = "Pause")))),
        xaxis=list(ticks="",
                   showticklabels = FALSE,
                   showgrid = FALSE,
                   title=""
        ),
        yaxis=list(ticks="",
                   showticklabels = FALSE,
                   showgrid = FALSE,
                   title=""
        ),
        margin = list(autoexpand=TRUE,t=65),
        plot_bgcolor  = "lightcyan",
        paper_bgcolor = "transparent",
        title=list(text ='Piracy acts and hostile actions against commercial shipping 1978-2023')
      ) %>%
      animation_button(label = "Play", pad = list(t = 42, r = 13),x=0.,y=0.1,
                       bgcolor = "white",   
                       # color = "red"
                       activebgcolor = "white"
      ) %>% 
      animation_slider(currentvalue = list(prefix = "YEAR: ", font = list(color =
                                                                            "grey10")))
    
  })
  output$barPlot_p <- renderPlotly({
    
    g2<-ggplot(filtered_data() %>% 
                 group_by(year) %>% 
                 summarise(n=n()),
               aes(as.factor(year),n,text=n))+
      geom_bar(stat='identity',fill='grey21')+
      theme(
        axis.text.x = element_text(
          angle = 60,
          hjust = 1,
          
        ))
    
    g2<-ggplotly(g2, tooltip = "text")
    g2 %>% 
      layout(
        xaxis=list(showgrid=F,title='year'),
        yaxis=list(title='number of incidents',gridcolor='grey10'),
        title=list(text='Number of piracy acts and hostile actions against commercial shipping by year'),
        margin = list(autoexpand=TRUE,t=65),
        plot_bgcolor='lightcyan',
        paper_bgcolor = "transparent"
      )
    
  })
  
  output$violinPlot_p <- renderPlotly({
    
    g5<-ggplot(filtered_data() %>% 
                 group_by(month,year) %>% 
                 summarise(n=n()),
               aes(month,n))
    if(input$type)
    {g5<-g5+geom_violin(fill='grey35')}
    else 
    {g5<-g5+geom_boxplot(fill='grey35')}
    g5<-ggplotly(g5,tooltip = "text")
    g5 %>% 
      layout(
        title =list(text='Distribution of yearly piracy acts and hostile actions against commercial shipping\n with grouping by month'),
        margin= list(autoexpand=TRUE,t=100),
        plot_bgcolor='lightcyan',
        paper_bgcolor = "transparent",
        yaxis=list(title='number of incidents',gridcolor='grey10',fixedrange = TRUE),
        xaxis=list(title='month',gridcolor='grey10',fixedrange = TRUE)
      )
  })
  
  output$dayPlot_p <- renderPlotly({
    g6<-ggplot(filtered_data() %>% 
                 group_by(day,year) %>% 
                 summarise(n=n()),
               aes(day,n))+theme(panel.grid.minor = element_line(colour = "grey10"),
                                 panel.grid.major = element_line(colour = "grey10"))
    if(input$type)
    {g6<-g6+geom_violin(fill='grey35')}
    else 
    {g6<-g6+geom_boxplot(fill='grey35')}
    g6<-g6+ scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    g6<-ggplotly(g6,tooltip = "text")
    g6 %>% 
      layout(
        title =list(text='Distribution of yearly piracy acts and hostile actions against commercial shipping\n with grouping by day of the week'),
        margin= list(autoexpand=TRUE,t=100),
        plot_bgcolor='lightcyan',
        paper_bgcolor = "transparent",
        yaxis=list(title='number of incidents',gridcolor='grey10',fixedrange = TRUE),
        xaxis=list(title='day',gridcolor='grey10',fixedrange = TRUE)
      )
  })

}

# App UI with navbar

app_ui <- navbarPage(
  title = "Maritime Transport",
  tabPanel("Introduction",ui0),
  tabPanel("Global Harbours", ui1),
  tabPanel("US Ocean Traffic", ui2),
  tabPanel("Piracy Acts", ui3),
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  2024 Ewelina Preś, Magdalena Sień, Maksymilian Wojciechowski
                  <a class='text-dark'</a>
                </p>
                </footer>
                "),
  header = tags$head(tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css"))
)

shinyApp(app_ui, server)