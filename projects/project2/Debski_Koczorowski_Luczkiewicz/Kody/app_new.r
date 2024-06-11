library(openxlsx)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(maps)
library(mapdata)
library(plotly)
library(shiny)
library(bslib)
library(readxl)
library(eurostat)
library(leaflet)
library(sf)
library(htmltools)
################################################################################

# FUNKCJE -----------------------------------------------------------------

row_to_names <- function(df){
  names <- as.character(unlist(df[1, ]))
  colnames(df) <- names
  df <- df[-1, ]
}

read_eurostat <- function(sheet){
  read_excel('tran_sf_railvi__custom_11230338_spreadsheet.xlsx', sheet = sheet)[c(10, 13:44), ] %>% 
    row_to_names() 
}

read_eurostat_2 <- function(){
  read_excel('tran_sf_railsu$defaultview_spreadsheet.xlsx', sheet = "Sheet 1")[c(7, 10:38), ] %>% 
    row_to_names() -> df
  
  names(df)[1] <- "Country"
  df
}


# WCZYTYWANIE DANYCH ------------------------------------------------------




iso<-read.xlsx("ISO_codes.xlsx")


# wypadki na drogach ze wzgledu na typ drogi 

drm<-read.xlsx("drogi_wypadki_miejskie.xlsx")
drw<-read.xlsx("drogi_wypadki_wiejskie.xlsx")
drn<-read.xlsx("drogi_wypadki_nieznane.xlsx")
dra<-read.xlsx("drogi_wypadki_autostrada.xlsx")
dr<-read.xlsx("drogi_wypadki.xlsx")

dr <- dr%>% mutate_all(as.character)
drm <- drm%>% mutate_all(as.character)
drw <- drw%>% mutate_all(as.character)
drn <- drn%>% mutate_all(as.character)
dra<-  dra%>% mutate_all(as.character)


drogi <- dr %>%
  pivot_longer(!c(TIME), names_to = 'Year', values_to = 'Total') %>%   
  transmute(Country=TIME,Year=as.numeric(Year),Total=as.numeric(Total)) %>%
  mutate(.by=Country,Mean_t= round(mean(Total,na.rm=T)))

drogi_miejskie<- drm %>%
  pivot_longer(!c(TIME), names_to = 'Year', values_to = 'Urban') %>%
  transmute(Country=TIME,Year=as.numeric(Year),Urban=as.numeric(Urban))%>%
  mutate(.by=Country,Mean_u= round(mean(Urban,na.rm=T)))

drogi_wiejskie<- drw %>%
  pivot_longer(!c(TIME), names_to = 'Year', values_to = 'Rural') %>%
  transmute(Country=TIME,Year=as.numeric(Year),Rural=as.numeric(Rural))%>%
  mutate(.by=Country,Mean_r= round(mean(Rural,na.rm=T)))

drogi_nieznane<-drn%>%
  pivot_longer(!c(TIME), names_to = 'Year', values_to = 'Unknown') %>%   
  transmute(Country=TIME,Year=as.numeric(Year),Unknown=as.numeric(Unknown))%>%
  mutate(.by=Country,Mean_un= round(mean(Unknown,na.rm=T)))

drogi_autostrada<-dra%>%
  pivot_longer(!c(TIME), names_to = 'Year', values_to = 'Motorway') %>%   
  transmute(Country=TIME,Year=as.numeric(Year),Motorway=as.numeric(Motorway))%>%
  mutate(.by=Country,Mean_m= round(mean(Motorway,na.rm=T)))

drogi_eu<-drogi%>%
  left_join(drogi_autostrada,by=c('Country','Year'))%>%
  left_join(drogi_miejskie,by=c('Country','Year'))%>%
  left_join(drogi_wiejskie,by=c('Country','Year'))%>%
  left_join(drogi_nieznane,by=c('Country','Year'))%>%
  left_join(iso,by=c('Country'))

dr0<-read.xlsx("drogi__wiek_na_mln_0.xlsx")
dr15<-read.xlsx("drogi__wiek_na_mln_15.xlsx")
dr18<-read.xlsx("drogi__wiek_na_mln_18.xlsx")
dr25<-read.xlsx("drogi__wiek_na_mln_25.xlsx")
dr50<-read.xlsx("drogi__wiek_na_mln_50.xlsx")
dr65<-read.xlsx("drogi__wiek_na_mln_65.xlsx")
drt<-read.xlsx("drogi__wiek_na_mln_total.xlsx")

dr0 <- dr0%>% mutate_all(as.character)
dr15 <- dr15%>% mutate_all(as.character)
dr18 <- dr18%>% mutate_all(as.character)
dr25 <- dr25%>% mutate_all(as.character)
dr50 <- dr50%>% mutate_all(as.character)
dr65 <- dr65%>% mutate_all(as.character)
drt <- drt%>% mutate_all(as.character)


d0 <- dr0 %>%
  pivot_longer(!c(TIME), names_to = 'Year', values_to = 'lt0') %>%   
  transmute(Country=TIME,Year=as.numeric(Year),'<15' =as.numeric(lt0))
d01 <- dr15 %>%
  pivot_longer(!c(TIME), names_to = 'Year', values_to = 'lt15') %>%   
  transmute(Country=TIME,Year=as.numeric(Year),'15-18'=as.numeric(lt15))
d1 <- dr18 %>%
  pivot_longer(!c(TIME), names_to = 'Year', values_to = 'lt18') %>%   
  transmute(Country=TIME,Year=as.numeric(Year),'19-25' =as.numeric(lt18))
d2 <- dr25 %>%
  pivot_longer(!c(TIME), names_to = 'Year', values_to = 'lt25') %>%   
  transmute(Country=TIME,Year=as.numeric(Year),'26-50' =as.numeric(lt25))
d3 <- dr50 %>%
  pivot_longer(!c(TIME), names_to = 'Year', values_to = 'lt50') %>%   
  transmute(Country=TIME,Year=as.numeric(Year),'51-65' =as.numeric(lt50))
d4 <- dr65 %>%
  pivot_longer(!c(TIME), names_to = 'Year', values_to = 'lt65') %>%   
  transmute(Country=TIME,Year=as.numeric(Year),'65<' =as.numeric(lt65))

drogi_wiek<-d0%>%
  left_join(d1,by=c('Country','Year'))%>%
  left_join(d01,by=c('Country','Year'))%>%
  left_join(d2,by=c('Country','Year'))%>%
  left_join(d3,by=c('Country','Year'))%>%
  left_join(d4,by=c('Country','Year'))%>%
  left_join(iso,by=c('Country'))
zmienna1<-c("Motorway","Urban","Rural", "All")
zmienna2<-c('<15','15-18','19-25','26-50','51-65','65<')


### WYPADKI KOLEJOWE

collisions_kill <- read_eurostat("Sheet 1") %>%
  mutate_at(2:11, as.numeric) %>% 
  mutate(type = "collision",
         harm = "death")

collisions_inj <- read_eurostat("Sheet 8") %>%
  mutate_at(2:11, as.numeric) %>% 
  mutate(type = "collision",
         harm = "injury")

derail_kill <- read_eurostat("Sheet 29") %>%
  mutate_at(2:11, as.numeric) %>% 
  mutate(type = "derailment",
         harm = "death")

derail_inj <- read_eurostat("Sheet 36") %>%
  mutate_at(2:11, as.numeric) %>% 
  mutate(type = "derailment",
         harm = "injury")

fire_kill <- read_eurostat("Sheet 43") %>%
  mutate_at(2:11, as.numeric) %>% 
  mutate(type = "fire",
         harm = "death")

fire_inj <- read_eurostat("Sheet 50") %>%
  mutate_at(2:11, as.numeric) %>% 
  mutate(type = "fire",
         harm = "injury")

trains <- rbind(collisions_kill, collisions_inj,
                derail_kill, derail_inj,
                fire_kill, fire_inj)

colnames(trains)[1] <- "Country" 

suicides <- read_eurostat_2()

colors_5 <- c("#59A5D8", "#09814A", "#2E5077", "#ACECA1", "#07004D")
################################################################################
# APLIKACJA 

################################################################################
# Pierwsza zakładka 

ui1 <- fluidPage(
  titlePanel("Transport accidents in Europe"),
  #textOutput('text1'),
  fluidRow(
    column(6, 
           selectizeInput('kraj1', 
                       'Select countries:',
                       unique(drogi_eu$Country),
                       selected=c('Poland','Italy','Germany'),
                       multiple = TRUE,
                       options = list(maxItems = 5)),
           selectInput('rok2', 
                       'Select year:',
                       selected ='2019',
                       unique(drogi_eu$Year))
           
    ),
    column(6,
           htmlOutput('text_wykresy_1'))
  ),
  fluidRow(
    column(6,
           shinycssloaders::withSpinner(plotlyOutput("barPlot"),
                                        type = getOption("spinner.type", default = 1),
                                        color = getOption("spinner.color", default = "#0275D8"),
                                        size = getOption("spinner.size", default = 1)
    )),
    column(6,
         shinycssloaders::withSpinner(plotlyOutput("linesPlot"),
                                      type = getOption("spinner.type", default = 1),
                                      color = getOption("spinner.color", default = "#0275D8"),
                                      size = getOption("spinner.size", default = 1)
    )
  )
  )
  )
################################################################################
# Druga zakładka 
ui2<- fluidPage(
  titlePanel("Road transport accidents"),
  #textOutput('text2'),
  fluidRow(
    column(6,
           selectInput('rok', 
                       'Select year:',
                       selected ='2019',
                       unique(drogi_eu$Year))),
    column(6,
           selectizeInput('kraj2', 
                          'Select countries:',
                          unique(drogi_eu$Country),
                          selected=c('Poland', 'Italy', 'Germany'),
                          multiple = TRUE,
                          options = list(maxItems = 5)))
    ),
  
  fluidRow(
    column(6,
           htmlOutput('text_wykresy_2')
           
    ),
    column(6,
             shinycssloaders::withSpinner(plotlyOutput("bar2Plot") ,
                                          type = getOption("spinner.type", default = 1),
                                          color = getOption("spinner.color", default = "#0275D8"),
                                          size = getOption("spinner.size", default = 1)
             ),
            
    )
  ),
  
  fluidRow(
    column(6,
           selectInput('wiek', 
                       'Select age:',
                       zmienna2,
                       selected= '<15',
           ),
           
           shinycssloaders::withSpinner(leafletOutput("map3Plot") ,
                                        type = getOption("spinner.type", default = 1),
                                        color = getOption("spinner.color", default = "#0275D8"),
                                        size = getOption("spinner.size", default = 1)
           )
    ),
    column(6,
           selectInput('typ', 
                       'Select road type:',
                       zmienna1,
                       selected="Urban"
                       ),
           shinycssloaders::withSpinner(leafletOutput("map2Plot") ,
                                        type = getOption("spinner.type", default = 1),
                                        color = getOption("spinner.color", default = "#0275D8"),
                                        size = getOption("spinner.size", default = 1)
           )
    )
  )
)
################################################################################
# Trzecia zakładka 
ui3<- fluidPage(
  titlePanel("Railway transport accidents"),
  htmlOutput('text3'),
  fluidRow(
    column(5,
           selectInput('rok_kolej', 
                       'Select year',
                       as.character(2013:2022),
                       selected = '2013',
                       multiple = FALSE),
           selectInput('uszkodzenie_kolej', 
                       'Select type of harm',
                       unique(trains$harm),
                       selected='death',
                       multiple = FALSE),
           htmlOutput('text_wykresy_3')
    ),
    column(7,
           shinycssloaders::withSpinner(plotlyOutput("trains_victims_Plot"),
                                                                           type = getOption("spinner.type", default = 1),
                                                                           color = getOption("spinner.color", default = "#0275D8"),
                                                                           size = getOption("spinner.size", default = 1)
           )
           )
  ),
  fluidRow(
    column(5,
           selectizeInput('panstwo_kolej',
                       'Select countries',
                       unique(suicides$Country),
                       selected = c("Poland", "Czechia", "Ireland"),
                       multiple = TRUE,
                       options = list(maxItems = 5)),
           htmlOutput('text_wykresy_4')
    ),
    column(7,
           shinycssloaders::withSpinner(plotlyOutput("trains_suicides_Plot"),
                                        type = getOption("spinner.type", default = 1),
                                        color = getOption("spinner.color", default = "#0275D8"),
                                        size = getOption("spinner.size", default = 1)
           )
    )
  )
)
################################################################################

server <- function(input, output) {
  
  output$table <- renderTable({
    
  })
  
  output$text1 <- renderText({
    paste("Application includes an analysis of accidents in European countries.")
  })
  output$text2 <- renderText({
    paste("Application includes an analysis of accidents in European countries.")
  })
  output$text3 <- renderUI({
    HTML("<strong>This page is dedicated to railway accidents.</strong>")
  })
  output$text_wykresy_1 <- renderUI({
    HTML("<strong>Choose up to 5 countries to compare data</strong>.<br/><br/>
         Plot on the left shows the number of victims in the selected year by type of road the accident happened on. 
         On the right plot you can see the total number of road accidents' victims in each country throughout the years.")
  })
  
  output$text_wykresy_2 <- renderUI({
    HTML("Each map and the bar chart visualise the data based on the selected year. Selecting a different year will affect all plots on this page.<br/><br/> 
    <strong>Use other select options to modify the plots.</strong>")
  })
  
  output$text_wykresy_3 <- renderUI({
    "Chart on the right shows the number of vicims in the selected year.
    You can choose between killed and injured victims."
  })
  
  output$text_wykresy_4 <- renderUI({
    "On this plot you can compare selected countries based on the number of
    suicides involving trains."
  })
  output$map2Plot <- renderLeaflet({
    
    mapa <- get_eurostat_geospatial(nuts_level = 0, year = 2003,
                                    resolution = '10')
    df <- as.data.frame(cbind(c("Sweden", "Netherlands", "United Kingdom", "Luxembourg", "Poland", "Czechia", "Germany", 
                                "Liechtenstein", "Denmark", "Lithuania", "Ireland", "Belgium", "Slovakia", "Austria", 
                                "Hungary", "Croatia", "Bulgaria", "Turkey", "Portugal", "Switzerland", "Slovenia", "Romania", 
                                "Italy", "Malta", "Cyprus", "Greece", "Spain", "Iceland", "Finland", "Estonia", "Latvia", "Norway", "France"),
                              c(mapa$id)))
    colnames(df) <- c("Name", "Code")
    
    mapa2 <- inner_join(mapa,df, by = join_by(id == Code)) %>% 
      select(Name, geometry) %>% 
      filter(Name != "Turkey")
    
    mapa_drogi <- inner_join(mapa2, drogi_eu, by= join_by(Name == Country)) %>% 
      select(-Mean_t, -Mean_m, -Mean_u, -Kraj, -Unknown, -Mean_un, -ISO, -Mean_r) %>% 
      st_drop_geometry() %>% 
      pivot_longer(!c(Name, Year), names_to = "Type", values_to = "Val")
    
    
    if (input$typ != "All"){
      mapa_drogi2 <- mapa_drogi %>% 
        filter(Year == input$rok, Type == input$typ)
    }
    else{
      mapa_drogi2 <- mapa_drogi %>% 
        filter(Year == input$rok, Type == "Total")
    }
    
    
    bins <- c(0, 10, 50, 100, 200, 1000, 2000, 3000,Inf)
    pal <- colorBin("YlOrRd", domain = mapa_drogi2$Val, bins = bins)

    labels <- lapply(ifelse(mapa_drogi2$Name == "United Kingdom" & input$rok >= 2019, sprintf("<strong>Not part of the EU since 2019") ,sprintf("Country: %s <br>Casualties: %d <br>Road Type: %s <br>Year: %s", mapa_drogi2$Name, mapa_drogi2$Val,ifelse(mapa_drogi2$Type != "Total", mapa_drogi2$Type, "All"), input$rok)),htmltools::HTML)
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
      setView(10,50, zoom = 2) %>% 
      addTiles() %>% 
      addPolygons(data = mapa2,
                  fillColor = pal(mapa_drogi2$Val),
                  weight = 1,
                  opacity = 1,
                  color = "black",
                  dashArray = 3,
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.7,
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>% 
      addLegend(pal = pal, values = mapa_drogi2$Val, opacity = 0.7,
                title = lapply("Number of casualties <br>(by road type)", htmltools::HTML),
                position = "bottomleft")
    
    
    
    
  })
  output$linesPlot <- renderPlotly({
    
    n <- length(input$kraj1)
    
    ggplotly(
      drogi_eu%>%
        filter(drogi_eu$Country %in% input$kraj1)%>%
        mutate(`Number of victims` = Total) %>% 
               ggplot(aes(x=Year,y=`Number of victims`,color=Country))+
               geom_line(linewidth=1)+
        labs(title='Number of victims through the years (1999-2022)',
             x = "Year",
             y = "")+
        theme(plot.background = element_rect(fill = "#102434", color = "#102434"),
              text = element_text(color = "#ffffff"),
              panel.background = element_rect(fill = "#ffffff"),
              axis.text = element_text(color = "#ffffff"),
              legend.background = element_rect(fill = "#102434", color = "#102434"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_line(color = "#efefef"),
              panel.grid.minor.y = element_line(color = "#efefef"))+
        scale_color_manual(values = colors_5[1:n])+
        scale_y_continuous(n.breaks = 10)
      )
  })
  output$barPlot <- renderPlotly({
    n <- length(input$kraj1)
    
    drogi3<- drogi_eu%>%select(Country,Year,Motorway,Urban,Rural)%>%
      filter(drogi_eu$Year==input$rok2,
             drogi_eu$Country %in% input$kraj1)%>%
      pivot_longer(!c(Country,Year), names_to = 'Road_type', values_to = 'Values')
    
    max_2 <- max(drogi3$Values, na.rm = TRUE)
    
    ggplotly(
        ggplot(drogi3,aes(x=Road_type,y=Values,fill=Country,
                          text = paste0("Road type: ", Road_type,
                                        "\nNumber of victims: ", Values,
                                        "\nCountry: ", Country)))+
        geom_col(position='dodge')+
          labs(title=sprintf('Number of victims in %s by type of road', input$rok2),
               x = "Road type",
               y = "")+
          theme(plot.background = element_rect(fill = "#102434", color = "#102434"),
                text = element_text(color = "#ffffff"),
                panel.background = element_rect(fill = "#ffffff"),
                axis.text = element_text(color = "#ffffff"),
                legend.background = element_rect(fill = "#102434", color = "#102434"),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_line(color = "#efefef"),
                panel.grid.minor.y = element_line(color = "#efefef"))+
          scale_fill_manual(values = colors_5[1:n])+
          scale_y_continuous(limit = c(0, max_2*1.05), expand = c(0, 0), n.breaks = 10),
        tooltip = "text"
        )
  })
  output$bar2Plot <- renderPlotly({
    n <- length(input$kraj2)
    
    drogi4<- drogi_wiek%>%select(Country,Year,`<15`,`15-18`,`19-25`,`26-50`,`51-65`,`65<`)%>%
      filter(drogi_wiek$Year==input$rok,
             drogi_wiek$Country %in% input$kraj2)%>%
      pivot_longer(!c(Country,Year), names_to = 'Victim', values_to = 'Values')
    
    max <- max(drogi4$Values, na.rm = TRUE)
    
    ggplotly(
      ggplot(drogi4,aes(x=Victim,y=Values,fill=Country,
                        text = paste0("Age: ", Victim,
                                      "\nNumber of victims: ", Values,
                                      "\nCountry: ", Country)))+
        geom_col(position='dodge')+
        labs(title=sprintf('Number of victims in %s by age', input$rok),
             x = "Age",
             y = "")+
        theme(plot.background = element_rect(fill = "#102434", color = "#102434"),
              text = element_text(color = "#ffffff"),
              panel.background = element_rect(fill = "#ffffff"),
              axis.text = element_text(color = "#ffffff"),
              legend.background = element_rect(fill = "#102434", color = "#102434"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_line(color = "#efefef"),
              panel.grid.minor.y = element_line(color = "#efefef"))+
        scale_fill_manual(values = colors_5[1:n])+
        scale_y_continuous(limit = c(0, max*1.05), expand = c(0, 0), n.breaks = 10),
      tooltip = "text"
      )
  })
  output$map3Plot <- renderLeaflet({
    
    mapa <- get_eurostat_geospatial(nuts_level = 0, year = 2003,
                                    resolution = '10')
    df <- as.data.frame(cbind(c("Sweden", "Netherlands", "United Kingdom", "Luxembourg", "Poland", "Czechia", "Germany", 
                                "Liechtenstein", "Denmark", "Lithuania", "Ireland", "Belgium", "Slovakia", "Austria", 
                                "Hungary", "Croatia", "Bulgaria", "Turkey", "Portugal", "Switzerland", "Slovenia", "Romania", 
                                "Italy", "Malta", "Cyprus", "Greece", "Spain", "Iceland", "Finland", "Estonia", "Latvia", "Norway", "France"),
                              c(mapa$id)))
    colnames(df) <- c("Name", "Code")
    
    mapa2 <- inner_join(mapa,df, by = join_by(id == Code)) %>% 
      select(Name, geometry) %>% 
      filter(Name != "Turkey")
    
    mapa_wiek <- inner_join(mapa2, drogi_wiek, by= join_by(Name == Country)) %>% 
      select(!c(Kraj, ISO)) %>% 
      st_drop_geometry() %>% 
      pivot_longer(!c(Name, Year), names_to = "Age", values_to = "Val")
    
    mapa_wiek2 <- mapa_wiek %>% 
        filter(Year == input$rok, Age == input$wiek)
  
    bins <- c(0, 10, 25, 50, 75, 100, 150, 200,Inf)
    pal <- colorBin("YlOrRd", domain = mapa_wiek2$Val, bins = bins)
    
    labels <- lapply(ifelse(mapa_wiek2$Name == "United Kingdom" & input$rok >= 2019, sprintf("<strong>Not part of the EU since 2019") ,sprintf("Country: %s <br>Casualties: %d <br>Age group: %s <br>Year: %s", mapa_wiek2$Name, mapa_wiek2$Val,
                                                                                                                                               case_when(
                                                                                                                                                 mapa_wiek2$Age == "<15" ~ "Children (under 15)",
                                                                                                                                                 mapa_wiek2$Age == "15-18" ~ "Teenagers (15-18)",
                                                                                                                                                 mapa_wiek2$Age == "19-25" ~ "Young adults (19-25)",
                                                                                                                                                 mapa_wiek2$Age == "26-50" ~ "Adults (26-50)",
                                                                                                                                                 mapa_wiek2$Age == "51-65" ~ "Seniors (51-65)",
                                                                                                                                                 mapa_wiek2$Age == "65<" ~ "Elderly (over 65)"
                                                                                                                                               ), input$rok)),htmltools::HTML)
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
      setView(10,50, zoom = 2) %>% 
      addTiles() %>% 
      addPolygons(data = mapa2,
                  fillColor = pal(mapa_wiek2$Val),
                  weight = 1,
                  opacity = 1,
                  color = "black",
                  dashArray = 3,
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.7,
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>% 
      addLegend(pal = pal, values = mapa_wiek2$Val, opacity = 0.7,
                title = lapply("Number of casualties <br>(by age group)", htmltools::HTML),
                position = "bottomleft")
    
  })
  
  
  output$trains_victims_Plot <- renderPlotly({
    rok_kolej <- input$rok_kolej
    uszkodzenie <- input$uszkodzenie_kolej
    
    trains[, c("Country", "type", "harm", rok_kolej)] -> df
    names(df)[4] <- "rok"
    
    df %>% 
      filter(harm == uszkodzenie) %>% 
      group_by(Country) %>% 
      summarise(sum = sum(rok)) %>% 
      select(sum) %>% max(na.rm = TRUE) -> limit_k
    
    df %>% 
      filter(harm == uszkodzenie) %>% 
      ggplot(aes(x = Country, y = rok, fill = type,
                       text = paste0("Country: ", Country,
                                     "\nNumber of victims: ", rok)))+
      geom_col(position = "stack")+
      labs(title = sprintf("Number of victims in rail accidents in %s", rok_kolej),
           x = "",
           y = "",
           fill = "Type of accident")+
      scale_fill_manual(values = c("#59A5D8", "#09814A","#2E5077"),
                        labels = c("collision", "derailment", "fire"))+
      scale_y_continuous(limits = c(0, limit_k*1.05), expand = c(0, 0), n.breaks = 10)+
      theme(plot.background = element_rect(fill = "#102434", color = "#102434"),
            text = element_text(color = "#ffffff"),
            panel.background = element_rect(fill = "#ffffff"),
            axis.text = element_text(color = "#ffffff"),
            legend.background = element_rect(fill = "#102434", color = "#102434"),
            axis.text.x = element_text(angle = 60),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(color = "#efefef"),
            panel.grid.minor.y = element_line(color = "#efefef"))-> plot
    
    ggplotly(plot, tooltip = "text")
  })
  
  output$trains_suicides_Plot <- renderPlotly({
    panstwo_kolej <- input$panstwo_kolej
    n <- length(panstwo_kolej)
    
    if (n > 1){
    label_t <- paste0(panstwo_kolej[1:(n-1)], collapse = ", ")
    label_t <- paste0(label_t, " and ", panstwo_kolej[n])
    }else{
      label_t <- panstwo_kolej
    }
    
    suicides %>% 
      filter(Country %in% panstwo_kolej) %>% 
      pivot_longer(as.character(2013:2022), values_to = "Number of Suicides", names_to = "Year") %>%
      mutate(`Number of Suicides` = as.numeric(`Number of Suicides`),
             Year = as.numeric(Year)) %>% 
      ggplot(aes(x = Year, y = `Number of Suicides`, color = Country))+
      geom_line(linewidth = 1)+
      scale_color_manual(values = colors_5[1:n])+
      labs(title = sprintf("Suicides over the years in %s.", label_t),
           x = "Year",
           y = "")+
      scale_x_continuous(breaks = 2013:2022, labels = as.character(2013:2022), expand = c(0, 0))+
      theme(plot.background = element_rect(fill = "#102434", color = "#102434"),
            text = element_text(color = "#ffffff"),
            panel.background = element_rect(fill = "#ffffff"),
            axis.text = element_text(color = "#ffffff"),
            legend.background = element_rect(fill = "#102434", color = "#102434"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(color = "#efefef"),
            panel.grid.minor.y = element_line(color = "#efefef"))+
      scale_y_continuous(n.breaks = 10)
    
      
      
  })
}


app_ui <- navbarPage(
  title = "Accidents in Europe",
  tabPanel("Introduction", ui1,
           icon = icon("database")),
  tabPanel("Road accidents", ui2,
           icon = icon("database")),
  tabPanel("Railway accidents", ui3,
           icon = icon("database")),

  theme = bslib::bs_theme(bootswatch = "superhero"))

shinyApp(app_ui, server)


