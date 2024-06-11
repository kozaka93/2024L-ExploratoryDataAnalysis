#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyr)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinyWidgets)
library(mapsapi)
library(leaflet)
library(tidyverse)
library(geojsonio)
library(shinythemes)

key = "AIzaSyCrBGAIAIGHvOZwlH1uIQMmGgkkUtiM8wI"

#pociagi <- read.csv("C:/Users/Admin/Downloads/Regularities_by_liaisons_Trains_France.csv")
pociagi <- read.csv("Regularities_by_liaisons_Trains_France.csv")
pociagi2 <- as.data.frame(pociagi)
France <- geojsonio::geojson_read("France.json", what = "sp")

#---------------- wykres drugi opoznienia ---------------------------------------------------------------------------------------------
pociagi3 <- pociagi2 %>% 
  mutate(trasa = paste(`Departure.station`, "-", `Arrival.station`))

pociagi3 <- pociagi3 %>% 
  select("Year", "Month", "trasa", "Number.of.expected.circulations", "Average.delay.of.all.departing.trains..min.", "Average.travel.time..min.") %>% 
  rename("oczekiwana_liczba_polaczen" = "Number.of.expected.circulations")

pociagi_op <- pociagi3 %>% 
  group_by(Year, Month) %>% 
  rename("srednie_opoznienie" = "Average.delay.of.all.departing.trains..min.") %>% 
  rename("sredni_czas_podrozy" = "Average.travel.time..min.")

pociagi_op <- pociagi_op %>%
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"))) %>% 
  arrange(Date)

#----------------- wykres pierwszy opoznienia ------------------------------------------------------------------------------------------
pociagi2 %>% 
  group_by(Year,Month) %>% 
  summarise(zewn = mean(Delay.due.to.external.causes, na.rm = TRUE),
            infras = mean(Delay.due.to.railway.infrastructure, na.rm = TRUE),
            ruch = mean(Delay.due.to.traffic.management, na.rm = TRUE),
            pojazdy = mean(Delay.due.to.rolling.stock,na.rm = TRUE),
            stacje = mean(Delay.due.to.station.management.and.reuse.of.material, na.rm = TRUE),
            pasazerowie = mean(Delay.due.to.travellers.taken.into.account, na.rm = TRUE)) -> opoznienia1

colnames(opoznienia1) <- c("Year","Month","Czynniki zewnętrzne", "Przebudowy infrastruktury kolejowej", "Zarządzanie i interakcje w ruchu", "Inne pojazdy na trasie",
                           "Zarządzanie stacją i materiałami", "Ruch pasażerski")

opoznienia1 <- opoznienia1 %>%
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"))) %>% 
  pivot_longer(cols = -c(Year, Month, Date), names_to = "DelayType", values_to = "Count")

#----------------- wykres rozkładu opóźnień po miesiącach w danym roku ---------------------------------------------------------------------------------------------------
miesiace <- c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec", "Lipiec",
              "Sierpień", "Wrzesień", "Październik", "Listopad", "Grudzień")
srednie_opoznienia <- pociagi %>%
  select(Number.of.expected.circulations, Average.delay.of.all.departing.trains..min., Year, Month) %>% 
  mutate(multiplier = Number.of.expected.circulations * Average.delay.of.all.departing.trains..min.) %>% 
  group_by(Year, Month) %>% 
  summarise(all_circulations = sum(Number.of.expected.circulations, na.rm = TRUE),
            all_multipliers = sum(multiplier, na.rm = TRUE)) %>%
  mutate(average_delay = all_multipliers/all_circulations)

#----------------- dane do mapy ---------------------------------------------------------------------------------------------------
mapa_trasy <- pociagi3 %>% 
  group_by(Year, trasa) %>% 
  summarise(oczekiwana_liczba_polaczen = sum(oczekiwana_liczba_polaczen, na.rm = TRUE))
trasy <- vector("list", length = 5)

#----------------- wykres pierwszy odwolania ---------------------------------------------------------------------------------------------------
pociagi4 <- pociagi %>% 
  rename("liczba_odwolanych" = "Number.of.cancelled.trains") %>% 
  arrange(Year)

#----------------- wykres liczba polaczen w miesiacu -------------------------------------------------------------------------------------------------
pociagi_barplot2 <- pociagi3 %>% 
  group_by(Year, Month) %>% 
  summarise(liczba_polaczen = sum(oczekiwana_liczba_polaczen))

# ---------------------- APLIKACJA ----------------------------------------------------------
home <- fluidPage(
  titlePanel("Wstęp"),
  shiny::markdown("Przedstawione wykresy sporządzono w oparciu o dane dotyczące pasażerskich połączeń kolejowych obsługiwanych przez pociągi \
  TGV (pociągi dużych prędkości) we Francji w latach 2015 – 06.2020. 
Przy analizie opóźnień przyjęto, że pociągi przybyły do stacji\ docelowej zgodnie z rozkładem, jeśli:
- przybyły w porę lub opóźnienie było mniejsze niż 5 minut dla połączeń, których czas podróży trwa poniżej 1h30 min.
- przybyły w porę lub opóźnienie było mniejsze niż 10 minut dla połączeń, których czas podróży trwa pomiędzy 1h30 min a 3h.
- przybyły w porę lub opóźnienie było mniejsze niż 15 minut dla połączeń, których czas podróży trwa powyżej 3h.
Czas przybycia jest zaokrąglany w dół do najbliższej pełnej minuty."),
  shiny::markdown("## Źródło danych \n
                  Wykorzystana ramka danych pochodzi ze strony Kaggle i znajduje się pod linkiem:\
                  https://www.kaggle.com/datasets/gatandubuc/public-transport-traffic-data-in-france."),
  imageOutput("home_img")
)

ui1 <- fluidPage(
    titlePanel("Przyczyny opóźnień w zależności od dat"),
    shiny::markdown("Poniższy wykres prezentuje procentowy udział różnych rodzajów przyczyn w opóźnieniach pociągów. Po lewej możemy wybrać zakres dat, jak i przyczyny, które chcemy zobaczyć na wykresie. \
                    Widzimy, że praktycznie we wszystkich latach dominują opóźnienia spowodowane przebudowami infrastruktury kolejowej oraz czynniki zewnętrzne. \
                    Widać też że w kwietniu 2020 roku bardzo widocznie wzrósł udział przebudow infratruktury kolejowej w przyczynach opóźnień."),
    fluidRow(column(3,
              wellPanel(
              pickerInput(inputId = "start_date", 
                          label = "Wybierz datę początkową:",
                          choices = format(unique(opoznienia1$Date), "%Y-%m"),
                          options = list(`format` = "yyyy-mm"),
                          selected = format(as.Date("2016-01-01"), "%Y-%m")),
              pickerInput(inputId = "end_date",
                          label = "Wybierz datę końcową:",
                          choices = format(unique(opoznienia1$Date), "%Y-%m"),
                          options = list(`format` = "yyyy-mm"),
                          selected = format(as.Date("2017-01-01"), "%Y-%m")),
              checkboxGroupInput("selected_delays", "Wybierz przyczyny opóźnienia:", 
                                 choices = c("Czynniki zewnętrzne", "Przebudowy infrastruktury kolejowej", "Zarządzanie i interakcje w ruchu", "Inne pojazdy na trasie", "Zarządzanie stacją i materiałami", "Ruch pasażerski"),
                                 selected = c("Czynniki zewnętrzne", "Przebudowy infrastruktury kolejowej", "Zarządzanie i interakcje w ruchu", "Inne pojazdy na trasie", "Zarządzanie stacją i materiałami", "Ruch pasażerski")))
              ),
             column(9,        
              mainPanel(
               plotlyOutput("linePlot"),
               width = 10)
                    )),
    titlePanel("Średnie opóźnienie w zależności od czasu podróży i liczby połączeń"),
    shiny::markdown("Poniższe wykresy przedstawiają zależności dla każdego miesiąca oddzielnie pomiędzy średnim czasem podróży, opóźnieniem i przewidywaną liczbą połączeń. \
                                          Można zauważyć prawidłowość, że przeważają połączenia do 200 minut, przy czym czas podróży od lipca 2017 roku dla tras o największej liczbie połączeń uległ skróceniu z ok. 200 minut od 130 – 150. \
                                          Charakterystycznym zjawiskiem jest wzrost opóźnień w miesiącach maj – sierpień/ wrzesień na trasach o największej liczbie połączeń z około 4 minut do około 8 minut. \
                                          Ponadto pociągi pokonujące najdłużej trwające trasy (niezbyt duża liczba planowanych połączeń) na ogół mają raczej małe opóźnienia: do 4 minut. "),
    
    fluidRow(column(3,
            wellPanel(
            pickerInput(inputId = "start_date1", 
                        label = "Wybierz datę:",
                        choices = format(unique(pociagi_op$Date), "%Y-%m"),
                        options = list(`format` = "yyyy-mm"),
                        selected = format(as.Date("2016-01-01"), "%Y-%m")))),
    column(9,        
           mainPanel(
             plotlyOutput("ScatterPlot"),
             width = 10))), 
             titlePanel("Średnie opóźnienia odjazdów pociągów we Francji"),
    shiny::markdown("Poniższy wykres prezentuje średnie opóźnienia pociągów we Francji w minutacch w poszczególnych miesiącach i latach. \
                    Zwykle znaczący wzrost opóźnień widać w okresie czerwiec-sierpień, czyli w okresie wakacyjnym. Również w grudniu da się dostrzec dość duży wzrost opóźnień w porównaniu do inncy hmiesięcy."),
    fluidRow(column(3, 
                    wellPanel(pickerInput(inputId = "year_nr_delay",
                                          label = "Wybierz rok:",
                                          choices = c(2015, 2016, 2017, 2018, 2019, 2020),
                                          selected = 2019))),
             column(9,        
                    mainPanel(
                      plotlyOutput("barplot3"),
                      width = 10)
             ))
    )

ui2 <- fluidPage(
  titlePanel("Rozkład liczby odwołanych pociągów w poszczególnych miesiącach"),
  shiny::markdown("Poniższy wykres przedstawia rozkład liczby odwołanych pociągów z podziałęm na miesiące i lata. Możemy wybrać zakres dat dla których chcemy porównać te rozkłady. \
                                                                        Widzimy rzeczywiście, że rzeczywiście liczba odwołanych pociagów w 2020 roku jest wyższa niż w wiekszości poprzednich, co prawdopodobnie było spowodowane pandemią Covid-19 (dla roku 2020 dostępne dane kończą się na czerwcu). \
                                                                        Natomiast najbardziej odstające dane widzimy dla roku 2018, co mogło zostać spowodowane licznymi protestami w kolejach francuskich właśnie w tamtym okresie."),
  fluidRow(column(3,
                  wellPanel(
                    checkboxGroupInput(inputId = "selected_year",
                                      label = "Wybierz lata",
                                      choices = unique(pociagi4$Year),
                                      selected = c(2018, 2019, 2020)))),
    column(9,
           mainPanel(
             plotOutput("boxplot"),
             width = 10))
    )
)

ui3 <- fluidPage(
  titlePanel("5 najbardziej popularnych tras w każdym miesiącu \n"),
  shiny::markdown("Dzięki poniższym wykresowi i mapie można dowiedzieć się, jakie trasy (miasto, dworzec początkowy – miasto, dworzec docelowy) były najpopularniejsze pod względem zaplanowanych połączeń. \
                  Informacje przedstawione są osobno dla każdego roku w podziale na miesiące i dotyczą pięciu tras z największą miesięczną liczbą zaplanowanych połączeń. \
                  Na podstawie wykresów można zauważyć, że najwięcej połączeń zostało zaplanowanych na trasie: Bordeaux St Jean – Paris Montparnasse. Jedynie w maju i czerwcu 2016 roku oraz kwietniu i maju 2020 r nie była to najpopularniejsza trasa. Poza nią najbardziej popularne były inne trasy z Paryża lub do Paryża, który jest stolicą oraz największym miastem we Francji, więc duża liczba połączeń na tych trasach jest spodziewana. \
                  W poszczególnych latach występuje jednak dość duże zróżnicowanie, jeśli chodzi o liczbę połączeń, co wskazuje na istotny wpływ czynników zewnętrznych; tylko niektóre dane mogą sugerować pewną sezonowość. \
                  Natomiast wykres na samym dole przedstawia całkowitą liczbę planowanych połączeń w podziale na miesiące w poszczególnych latach analizowanego okresu."),
  
  fluidRow(column(3,
                 wellPanel(pickerInput(inputId = "year_popular",
                                       label = "Wybierz rok",
                                       choices = unique(pociagi4$Year),
                                       selected = 2019))),
  column(9,
         mainPanel(plotlyOutput("barplot"), width = 10, height = 10))),
  titlePanel("Najpopularniejsze trasy na mapie"),

  fluidRow(column(3,
                   wellPanel(pickerInput(inputId = "year_map",
                                         label = "Wybierz rok:",
                                         choices = c(2015, 2016, 2017, 2018, 2019, 2020),
                                         selected = 2019))),
  column(9,
         mainPanel(leafletOutput("mapa"), width = 10))),
  titlePanel("Liczba połączeń w każdym miesiącu"),
  shiny::markdown("W większości widzimy, że liczby połączeń w każdym miesiącu, jak i roku są zbliżone. Jednak gwałtowny spadek możemy zobaczyć w grudniu 2019 z powodu strajku transportu publicznego. Spowodował on, że w niektóre dni kursowała jedynie 1/5 pociągów TGV. \
  Oczywiście widoczny spadek jest również w marcu 2020 roku ze względu na wybuch pandemii koronawirusa."),
  fluidRow(column(3, 
                  wellPanel(pickerInput(inputId = "year_nr_circ",
                                        label = "Wybierz rok:",
                                        choices = c(2015, 2016, 2017, 2018, 2019, 2020),
                                        selected = 2019))),
  column(9, 
         mainPanel(plotOutput("barplot2"), width = 10)))
)

# SERWER

server <- function(input, output) {
    output$linePlot <- renderPlotly({
      start_date <- as.Date(paste(input$start_date, "01", sep = "-"))
      end_date <- as.Date(paste(input$end_date, "01", sep = "-"))
      
      plot_ly(data = opoznienia1 %>% filter(Date >= start_date, Date <= end_date, DelayType %in% input$selected_delays),
              x = ~Date, y = ~Count, color = ~DelayType, mode = "line") %>% 
        layout(xaxis = list(title = "Data", tickformat = "%Y-%m", tickangle = 45),
              yaxis = list(title = "Liczba pociągów (w %)"), legend=list(title=list(text='<b> Przyczyny opóźnień </b>'))) %>% 
        add_trace(hovertemplate = "Data: %{x},<br>Wartość:: %{y}<extra></extra>")
        
  })
    
    output$ScatterPlot <- renderPlotly({
      start_date1 <- as.Date(paste(input$start_date1, "01", sep = "-"))
      
      fig <- plot_ly(pociagi_op %>% filter(Date == start_date1), x = ~srednie_opoznienie, y = ~sredni_czas_podrozy, type = 'scatter', mode = 'markers', color = ~oczekiwana_liczba_polaczen, colors = "Reds",
                     marker = list(size=18, opacity = 0.7, sizemode = 'diameter'),
                     text = ~paste('Nazwa trasy', trasa)) %>% 
       layout(title = 'Długość podróży a średnie opoznienia',
                     xaxis = list(title ="Średnie opóżnienie na trasie",
                                  titlefont = list(size = 13), showgrid = FALSE),
                     
                     yaxis = list(title ="Średni czas podróży", 
                                  titlefont = list(size = 13),
                                  showgrid = FALSE),
                     paper_bgcolor = 'rgb(243, 243, 243)',
                     plot_bgcolor = 'rgb(243, 243, 243)') %>% 
        colorbar(title = "Długość trasy", titlefont = list(size = 11))
      fig
    })
    
    output$boxplot <- renderPlot({
      ggplot(pociagi4 %>% filter(Year %in% input$selected_year), aes(x=as.factor(Month), y=liczba_odwolanych, fill = as.factor(Year)))+
        geom_boxplot()+
        theme_bw() +
        xlab("Miesiąc") +
        ylab("Rozkład liczby odwołanych na poszczególnych trasach") +
        labs(fill = "Rok")
    })
    
    output$barplot <- renderPlotly({
      # troche to dlugie wyszlo, ale nie wiem czy da sie to rozdzielic albo skrocic
      year1 <- input$year_popular
      pociagi6 <- pociagi3 %>% 
        filter(Year == year1) %>% 
        group_by(Month) %>% 
        arrange(desc(oczekiwana_liczba_polaczen), .by_group = TRUE) %>% 
        slice(1:1) 
      pociagi7 <- pociagi3 %>% 
        filter(Year == year1) %>% 
        group_by(Month) %>% 
        arrange(desc(oczekiwana_liczba_polaczen), .by_group = TRUE) %>% 
        slice(2:2) 
      pociagi8 <- pociagi3 %>% 
        filter(Year == year1) %>% 
        group_by(Month) %>% 
        arrange(desc(oczekiwana_liczba_polaczen), .by_group = TRUE) %>% 
        slice(3:3) 
      pociagi9 <- pociagi3 %>% 
        filter(Year == year1) %>% 
        group_by(Month) %>% 
        arrange(desc(oczekiwana_liczba_polaczen), .by_group = TRUE) %>% 
        slice(4:4) 
      pociagi10 <- pociagi3 %>% 
        filter(Year == year1) %>% 
        group_by(Month) %>% 
        arrange(desc(oczekiwana_liczba_polaczen), .by_group = TRUE) %>% 
        slice(5:5) 
      
      x <- as.factor(pociagi6$Month)
      y1 <- pociagi6$oczekiwana_liczba_polaczen
      y2 <- pociagi7$oczekiwana_liczba_polaczen
      y3 <- pociagi8$oczekiwana_liczba_polaczen
      y4 <- pociagi9$oczekiwana_liczba_polaczen
      y5 <- pociagi10$oczekiwana_liczba_polaczen
      data <- data.frame(x, y1, y2, y3, y4, y5)
      
      fig <- plot_ly(data, x = ~x, y = ~y1, type = 'bar', name = 'Najbardziej popularna', marker = list(color = 'rgb(167, 0, 0)'),
                     hovertext = ~paste('Nazwa:', pociagi6$trasa), hovertemplate = "Miesiąc: %{x}<br>Liczba przejazdów: %{y}<br>%{hovertext}<extra></extra>") %>% 
        add_trace(y = ~y2, name = 'Druga najbardziej popularna', marker = list(color = 'rgb(255, 0, 0)'),
                  hovertext = ~paste('Nazwa:', pociagi7$trasa), hovertemplate = "Miesiąc: %{x}<br>Liczba przejazdów: %{y}<br>%{hovertext}<extra></extra>") %>% 
        add_trace(y = ~y3, name = 'Trzecia najbardziej popularna', marker = list(color = 'rgb(255, 82, 82)'),
                  hovertext = ~paste('Nazwa:', pociagi8$trasa), hovertemplate = "Miesiąc: %{x}<br>Liczba przejazdów: %{y}<br>%{hovertext}<extra></extra>") %>% 
        add_trace(y = ~y4, name = 'Czwarta najbardziej popularna', marker = list(color = 'rgb(255, 123, 123)'),
                  hovertext = ~paste('Nazwa:', pociagi9$trasa), hovertemplate = "Miesiąc: %{x}<br>Liczba przejazdów: %{y}<br>%{hovertext}<extra></extra>") %>% 
        add_trace(y = ~y5, name = 'Piąta najbardziej popularna', marker = list(color = 'rgb(255, 186, 186)'),
                  hovertext = ~paste('Nazwa:', pociagi10$trasa), hovertemplate = "Miesiąc: %{x}<br>Liczba przejazdów: %{y}<br>%{hovertext}<extra></extra>") %>% 
        layout(xaxis = list(title = "Miesiąc", tickangle = -45),
               yaxis = list(title = "Liczba połączeń"),
               margin = list(b = 100),
               barmode = 'group')
    })
    
    output$barplot2 <- renderPlot({
    ggplot(pociagi_barplot2 %>%  filter(Year %in% input$year_nr_circ), aes(x = as.factor(Month), y = liczba_polaczen)) +
      geom_bar(stat = "identity", color = "black", fill="lightsalmon") +
      theme_bw() +
      theme( panel.grid.major.x = element_blank() ,
             panel.grid.major.y = element_line( size=.1, color="black" )) +
      xlab("Miesiąc") +
      ylab("Liczba połączeń") 
    })
    
    output$barplot3 <- renderPlotly({
      srednie_opoznienia2 <- filter(srednie_opoznienia, Year == input$year_nr_delay)
      if (input$year_nr_delay == 2020){
        plot_ly(data = srednie_opoznienia2, x = srednie_opoznienia2$Month, y = srednie_opoznienia2$average_delay,
                type = "bar", color = "lightsalmon", 
                hovertemplate = "Miesiąc: %{hovertext}<br>Średnie opóźnienie: %{y} minut<extra></extra>",
                hovertext = miesiace[1:6],
                marker = list(line = list(color = "black", width = 2))) %>% 
          layout(xaxis = list(title = "Miesiąc", tickvals = 1:6), 
                 yaxis = list(title = "Średni czas opóźnienia w minutach"), 
                 showlegend = FALSE)
      } else{
        plot_ly(data = srednie_opoznienia2, x = srednie_opoznienia2$Month, y = srednie_opoznienia2$average_delay,
                type = "bar", color = "lightsalmon", 
                hovertemplate = "Miesiąc: %{hovertext}<br>Średnie opóźnienie: %{y} minut<extra></extra>",
                hovertext = miesiace,
                marker = list(line = list(color = "black", width = 2))) %>% 
          layout(xaxis = list(title = "Miesiąc", tickvals = 1:12), 
                 yaxis = list(title = "Średni czas opóźnienia w minutach"), 
                 showlegend = FALSE)
      }
    })
    
    output$mapa <- renderLeaflet({
      mapa_trasy_temp <- mapa_trasy %>% 
        filter(Year == input$year_map) %>% 
        arrange(desc(oczekiwana_liczba_polaczen))
      mapa_trasy_temp <- slice_head(mapa_trasy_temp, n = 5)
      for (i in 1:5) {
        locations <- str_split(mapa_trasy_temp$trasa[i], " - ")
        directions <-  mp_directions(
          origin = locations[[1]][1],
          destination = locations[[1]][2],
          mode = "driving",
          alternatives = FALSE,
          key = key,
          quiet = TRUE
        )
        trasy[[i]] <- mp_get_routes(directions)
      }
      leaflet(France) %>% 
        addTiles() %>%
        setView(2, 46, 5) %>% 
        addPolygons(color = "black") %>% 
        addPolylines(data = trasy[[1]], opacity = 0.5, weight = 3, color = "black") %>% 
        addPolylines(data = trasy[[2]], opacity = 0.5, weight = 3, color = "black") %>% 
        addPolylines(data = trasy[[3]], opacity = 0.5, weight = 3, color = "black") %>% 
        addPolylines(data = trasy[[4]], opacity = 0.5, weight = 3, color = "black") %>% 
        addPolylines(data = trasy[[5]], opacity = 0.5, weight = 3, color = "black") 
    })
    
    output$home_img <- renderImage({
      
      list(src = "www/TGV.jpg",
           width = "600",
           height = "400")
      
    }, deleteFile = F)
}

# łączenie w jedną aplikację
app_ui <- navbarPage(
  tags$head(tags$style('body {font-family: Arial;}')),
  title = "",
  tabPanel("Strona główna", home, icon = icon("house")),
  tabPanel("Połączenia i trasy", ui3, icon = icon("train")),
  tabPanel("Opóźnienia", ui1, icon = icon("hourglass-start")),
  tabPanel("Odwołania", ui2, icon = icon("ban")),
  theme = shinytheme("united"),
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  Autorzy projektu: Maria Krawczyk, Julia Le Bihan, Jędrzej Lipowski
                </p>
                </footer>
                "))


# Run the app
shinyApp(ui = app_ui, server = server)

