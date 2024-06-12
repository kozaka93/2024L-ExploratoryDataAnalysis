library(shiny)
library(tidyr)
library(dplyr)
library(plotly)
library(openxlsx)
library(leaflet)
library(sf)
library(readxl)
library(ggplot2)
library(maps)
library(mapdata)
library(stringi)
df <- read.csv2("Punktualnosc_pasazerska_2023_III_kwartal.csv")
df <- df[-(1:4), -c(3, 4, 5, 11, 12)]


names(df) <- c(
  "Rok",
  "Miesiąc",
  "Opóźnienie do 5 minut i 59 sekund",
  "Opóźnienie od 6 minut do 59 minut i 59 sekund",
  "Opóźnienie od 60 minut do 119 minut i 59 sekund",
  "Opóźnienie powyżej 120 minut",
  "Pociągi odwołane"
)
df <- pivot_longer(
  df,
  cols = c(
    "Opóźnienie do 5 minut i 59 sekund",
    "Opóźnienie od 6 minut do 59 minut i 59 sekund",
    "Opóźnienie od 60 minut do 119 minut i 59 sekund",
    "Opóźnienie powyżej 120 minut",
    "Pociągi odwołane"
  ),
  names_to = "Rodzaj opóźnienia",
  values_to = "Liczba"
)
df$Miesiąc <- factor(df$Miesiąc, levels = unique(df$Miesiąc))




df2 <- read.xlsx(
  "Sredni_czas_opoznien_na_stacjach_dla_pociagow_opoznionych_powyzej_5_minut2021.xlsx"
)
df2 <- pivot_longer(
  df2,
  cols = c(
    "styczeń",
    "luty",
    "marzec",
    "kwiecień",
    "maj",
    "czerwiec",
    "lipiec",
    "sierpień",
    "wrzesień",
    "październik",
    "listopad",
    "grudzień"
  ),
  names_to = "Miesiąc",
  values_to = "Średni czas opóźnienia w minutach"
)

df2021 <- read.xlsx("Pomocnicze2021.xlsx")
df2022 <- read.xlsx("Wskaznik2022.xlsx")
df2023 <- read.xlsx("Wskaznik2023.xlsx")
df2022pom <- read.xlsx("Liczba_zatrzyman_pociagow_na_stacjach_w_2022.xlsx")
df2023pom <- read.xlsx("Liczba_zatrzyman_I_-_XII_2023.xlsx")
df2022pom <- df2022pom %>% select(c("Stacja", "Suma"))
df2023pom <- df2023pom %>% select(c("Stacja", "Suma"))
df2022 <- left_join(df2022, df2022pom, by = "Stacja")
df2023 <- inner_join(df2023, df2023pom, by = "Stacja")

df2021 <- pivot_longer(
  df2021,
  cols = c(
    "styczeń",
    "luty",
    "marzec",
    "kwiecień",
    "maj",
    "czerwiec",
    "lipiec",
    "sierpień",
    "wrzesień",
    "październik",
    "listopad",
    "grudzień"
  ),
  names_to = "Miesiąc",
  values_to = "Wskaźnik"
)

df2022 <- pivot_longer(
  df2022,
  cols = c(
    "styczeń",
    "luty",
    "marzec",
    "kwiecień",
    "maj",
    "czerwiec",
    "lipiec",
    "sierpień",
    "wrzesień",
    "październik",
    "listopad",
    "grudzień"
  ),
  names_to = "Miesiąc",
  values_to = "Wskaźnik"
)

df2023 <- pivot_longer(
  df2023,
  cols = c(
    "styczeń",
    "luty",
    "marzec",
    "kwiecień",
    "maj",
    "czerwiec",
    "lipiec",
    "sierpień",
    "wrzesień",
    "październik",
    "listopad",
    "grudzień"
  ),
  names_to = "Miesiąc",
  values_to = "Wskaźnik"
)

df2021$rok <- 2021
df2022$rok <- 2022
df2023$rok <- 2023
df3 <- rbind(df2021, df2022, df2023)
#-------------------------------------------------------------------------------

dt <- read_excel("Wymiana_pasazerska_2021_-_akt__ranking.xlsx")
op <- read_excel("Sredni_czas_opoznien_na_stacjach_dla_pociagow_opoznionych_powyzej_5_minut2021.xlsx")
op[,2:13]<- lapply(op[,2:13],as.numeric)
op$opóźnienie <- rowMeans(op[,2:13],na.rm = TRUE)
op <- op %>% select(stacje,opóźnienie)
dt <- left_join(dt,op, by = c("nazwa stacji" = "stacje"))
world <- map_data("world")
names(dt)[7] <- "wymiana"
names(dt)[2] <- "long2"
names(dt)[3] <- "lat2"
Polska <- world %>% filter(region == 'Poland')
dt<- dt %>% 
  mutate(wymiana_pasażerska = as.numeric(stri_extract_last_regex(wymiana, '\\b\\w+\\b$'))) %>% 
  mutate(long = as.numeric(long2),lat = as.numeric(lat2),`średnia pasażerów na zatrzymanie` = as.numeric(`średnia liczba pasażerów na 1 zatrzymanie  (wg progów zaokrągleń)`)) %>% 
  filter(long> 0)
duze <- dt %>% 
  filter(wymiana_pasażerska >1000)
duze <- duze$`nazwa stacji`
czynniki = c("wymiana_pasażerska","opóźnienie","średnia pasażerów na zatrzymanie","średnia dobowa liczba zatrzymań")


#-------------------------------------------------------------------------------

voivodeships <- c("Łódzkie", "Świętokrzyskie", "Wielkopolskie", "Kujawsko-pomorskie",
                  "Małopolskie", "Dolnośląskie", "Lubelskie", "Lubuskie",
                  "Mazowieckie", "Opolskie", "Podlaskie", "Pomorskie",
                  "Śląskie", "Podkarpackie", "Warmińsko-mazurskie", "Zachodniopomorskie")

userate_db <- read.xlsx("nawoj.xlsx")
colnames(userate_db) <- userate_db[1, ]
userate_db <- userate_db[-1, ]
names(userate_db) <- paste(toupper(substring(names(userate_db), 1, 1)),
                           substring(names(userate_db), 2), sep = "")
userate_db <- userate_db %>%
  filter(Kwartał != "ogółem") %>%
  mutate(rk = paste(Rok, " ", Kwartał))
userate_db <- userate_db[, c(20, 3:18)]
for (i in 2:17) {
  userate_db[, i] <- as.numeric(userate_db[, i])
}
userate_db_long <- gather(userate_db, region, value, -rk)

miscelanneous_db <- read.csv("miscelanneous_db.csv")

# Load voivodeship polygons
voivodeship_polygons <- st_read("polska-wojewodztwa.geojson")
voivodeship_polygons$name <- voivodeships

selected_voivodeships <- reactiveVal(voivodeships)

# Define color palette
voivodeship_names <- unique(voivodeship_polygons$name)
colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00",
            "#FFA500", "#800080", "#FFC0CB", "#40D0D0",
            "#A52A2A", "#808000", "#00EEFF", "#9966CC",
            "#0F52BA", "#FF7F50", "#E30B5D", "#98FF98")
names(colors) <- voivodeship_names


#-------------------------------------------------------------------------------

ui1 <- fluidPage(
  tags$head(
    tags$style(HTML("
      #map-container {
        position: fixed;
        width: 40%;
        height: 100%;
        z-index: 1; /* Ensure map is below other elements */
      }
      #KBgraphs-container {
        margin-left: 45%;
        margin-right: 0%;
        height: 100vh;
        padding: 10px;
        z-index: 2; /* Ensure KBgraphs are above map */
      }
      h1 {
        font-size: 24px;
      }
      .KBgraph-placeholder {
        font-size: 12px;
        color: gray;
        margin-top: 5%;
        margin-bottom: 5%;
      }
    "))
  ),
  div(id = "map-container",
      leafletOutput("map", width = "100%", height = "80%")
  ),
  div(id = "KBgraphs-container",
      h1("Wykresy:"),
      plotlyOutput("KBgraph1"),
      div(class = "KBgraph-placeholder", "."),
      hr(),
      plotlyOutput("KBgraph2"),
      hr(),
      plotlyOutput("KBgraph3"),
      hr(),
      plotlyOutput("KBgraph4"),
      hr(),
      plotlyOutput("KBgraph5"),
      hr()
  )
)

#-------------------------------------------------------------------------------

ui2 <- fluidPage(
  titlePanel("Linie kolejowe wychodzące z danego miasta"),
  
  
  fluidRow(
    column(6,
           selectInput("Miasto", "Linie kolejowe wychodzące ze stacji",duze),
           selectInput("Czynnik", "Wybierz czynnik sortujący",czynniki),
           textOutput("textM3"),
           textOutput("textM2")
    ),
    
    
    column(6,
           plotOutput("distPlotM2")
    )
  ),
  fluidRow(column(6,plotOutput("distPlotM3")),
           column(6,plotOutput("distPlotM4")))
)
#-------------------------------------------------------------------------------


ui3 <- fluidPage(
  titlePanel("Opoźnienia"),
  
  
  fluidRow(column(6, fluidRow(
    column(2, checkboxGroupInput(
      "rok", "Wybierz rok:", unique(df$Rok), 2019,
    )),
    column(
      6,
      sliderInput(
        "miesiace",
        "Wybierz zakres miesięcy:",
        min = 1,
        max = 12,
        value = c(3, 9),
        step = 1
      )
    ),
    
    column(
      4,
      selectInput(
        "rodzaj_opoznienia",
        "Wybierz rodzaj opóżnienia:",
        unique(df$`Rodzaj opóźnienia`)
      )
    )
  ))
  
  , column(6, fluidRow(
    column(6, selectInput(
      "Miesiąc", "Wybierz miesiąc:", unique(df2$Miesiąc)
    )), column(
      6,
      sliderInput(
        "kubełki",
        "Wybierz liczbę przedziałów:",
        min = 1,
        max = 100,
        value = 50
      )
    )
  ))),
  
  
  fluidRow(column(6, plotlyOutput("distPlot")), column(6, plotlyOutput("distPlot2"))),
  fluidRow(column(6, htmlOutput("htmlText")), column(6, htmlOutput("htmlText2")))
)

#-------------------------------------------------------------------------------

server <- function(input, output) {
  
  
  output$distPlot <- renderPlotly({
    ggplotly(ggplot(
      df %>% filter(
        Rok %in% input$rok,
        `Rodzaj opóźnienia` == input$rodzaj_opoznienia
      ) %>% group_by(Rok) %>% slice(input$miesiace[1]:input$miesiace[2]),
      aes(
        x = Miesiąc,
        y = Liczba,
        color = as.factor(Rok),
        group = as.factor(Rok)
      )
    ) + geom_point() + geom_line() + labs(x = "Miesiąc", y = "Liczba pociągów", color =
                                            "Rok:",title="Liczba pociągów z podziałem na poszczególne opóźnienia i lata")+theme(axis.text.x = element_text(
                                              angle = 45,  
                                              hjust = 1,  
                                              vjust = 1  
                                            )
                                            )) %>% layout(paper_bgcolor = "white",
                                                          plot_bgcolor = "white",yaxis = list(
                                                            
                                                            showgrid = TRUE,  
                                                            gridcolor = "lightgray")) %>% style(
                                                              hovertemplate = paste('<b>Miesiąc:</b> %{x}<br><b>Liczba:</b> %{y:.0f} <extra></extra>')  
                                                            )
    
    
    
  })
  output$distPlot2 <- renderPlotly({
    ggplotly(
      ggplot(
        df2 %>% filter(
          Miesiąc == input$Miesiąc,
          `Średni czas opóźnienia w minutach` < 100
        ),
        aes(x = `Średni czas opóźnienia w minutach`)
      ) +
        geom_histogram(bins = input$kubełki,fill="lightgreen",color="lightgreen") + labs(
          x = "Średni czas opóźnienia na stacji (w minutach)",
          y = "Liczba stacji",
          title = "Rozkład średniego opóźnienia w 2021 roku" 
        )+scale_y_continuous(expand = c(0,0))+scale_x_continuous(breaks = seq(0, 60, by = 10))
    ) %>% layout(paper_bgcolor = "white",
                 plot_bgcolor = "white",yaxis = list(
                   
                   showgrid = TRUE,  
                   gridcolor = "lightgray")) %>% style(
                     hovertemplate = paste('<b>Liczba stacji:</b> %{y:.0f} <extra></extra>')  
                   )
    
  })
   output$htmlText <- renderUI({HTML(
    
     "<p style=\"text-align: justify;\"> Powyższy interaktywny wykres umożliwia przyjrzenie się tendencji 
          dotyczącej opóźnień jaka panuje na kolei w ostatnich latach.
          Trzy zakładki pozwalają na wybór odpowiednio roku, miesiąca i rodzaju opóźnienia 
          (od tych najkrótszych aż do kursów odwołanych).
         Wykres umożliwia obserwacja róźnych tendencji na przestrzeni lat i miesięch takich jak na przykałd
          tendencja wzrostowa liczby opóźnień rok do roku we wszystkcich kategoriach opóźnień lub fakt znacznego
          wzrostu opóźnień na początku zimy tj. w listopadzie i grudniu.
     Ponadto wykres pozwala nam na obserwowanie różnych anomalii w liczbie opóźnień takich jak np. znaczny wzrost 
     liczby dużych opóźnień w marcu 2022 roku spowodowany wybuchem wojny na Ukrainie lub znaczny spadek opóźnień
     dwa lata wcześnie, którego przyczyną był początek pandemii COVID-19.</p>")
    
  })
   output$htmlText2 <- renderUI({HTML(
     
     "<p style=\"text-align: justify;\"> Drugi wykres obrazuje nam za pomocą histogramu
     rozkład średniego czasu opóźnień w roku 2021 (liczony jest tylko czas opóźnień powyżej 6 minut).
     Możemy zaobserwować ile stacji 
     w danym miesiącu miało średnie opóźnienie mieszczące się w odpowiednim przedziale.
    Interaktywność umożliwia nam wybór konkretnego miesiąca, a także liczbę przedziałów,
      na którą ma być podzielony histogram. Na podstawie wykresu możemy ocenić jaki był
     najczęstszy średni czas opóźnień (mieści się on zawsze w przedziale pomiędzy 10 a 20 minut) 
     i w którym miesiącu był on największy. Histogram
     umożliwia nam też obserwacje charakterystyki rozkładu i zauważenie, że w każdym
     miesiącu rozkład jest dodatnio asymetryczny z dużą liczbą outlierów związanych z wysokimi
     czasami opóźnień.</p>")
     
   })
   
   # ---
   # Create interactive map
   output$map <- renderLeaflet({
     leaflet(options = leafletOptions(zoomControl = FALSE, dragging = FALSE, scrollWheelZoom = FALSE, doubleClickZoom = FALSE, maxZoom = 6.7)) %>%
       setView(lng = 19, lat = 52, zoom = 6.7) %>%
       addPolygons(data = voivodeship_polygons,
                   fillOpacity = 0.5,
                   color = "black",
                   weight = 1,
                   layerId = ~name, 
                   label = ~name,
                   highlight = highlightOptions(
                     weight = 5,
                     color = "red",
                     fillOpacity = 0.7,
                     bringToFront = TRUE
                   ),
                   fillColor = ~colors[name])
   })
   
   # Update selected voivodeships when map is clicked
   observeEvent(input$map_shape_click, {
     clicked_shape <- input$map_shape_click
     if (!is.null(clicked_shape)) {
       clicked_id <- clicked_shape$id
       current_selection <- selected_voivodeships()
       if (clicked_id %in% current_selection) {
         current_selection <- setdiff(current_selection, clicked_id)
       } else {
         current_selection <- union(current_selection, clicked_id)
       }
       selected_voivodeships(current_selection)
     }
   })
   
   # Update the map based on the selected voivodeships
   observe({
     selected <- selected_voivodeships()
     if (length(selected) == 0) {
       leafletProxy("map", data = voivodeship_polygons) %>%
         clearShapes() %>%
         addPolygons(data = voivodeship_polygons,
                     fillOpacity = 0.5,
                     color = "black",
                     weight = 1,
                     layerId = ~name, 
                     label = ~name,
                     highlight = highlightOptions(
                       weight = 5,
                       color = "red",
                       fillOpacity = 0.7,
                       bringToFront = TRUE
                     ),
                     fillColor = "grey")
     } else {
       leafletProxy("map", data = voivodeship_polygons) %>%
         clearShapes() %>%
         addPolygons(data = voivodeship_polygons,
                     fillOpacity = 0.5,
                     color = "black",
                     weight = 1,
                     layerId = ~name, 
                     label = ~name,
                     highlight = highlightOptions(
                       weight = 5,
                       color = "red",
                       fillOpacity = 0.7,
                       bringToFront = TRUE
                     ),
                     fillColor = ~ifelse(name %in% selected, colors[name], "grey"))
     }
   })
   
   output$KBgraph1 <-  renderPlotly({
     selected_regions <- selected_voivodeships()
     plot_data <- userate_db_long %>% 
       filter(region %in% selected_regions)
     
     plot_ly(data = plot_data, x = ~rk, y = ~value, color = ~region, type = 'scatter', mode = 'lines',
             colors = colors) %>%
       layout(title = "Wskaźnik wykorzystania kolei",
              xaxis = list(title = "Kwartał"),
              yaxis = list(title = "Liczba podróży na 1 mieszkańca", tickformat = ".1f"),
              height = 500,
              width = 900, 
              legend = list(orientation = "v"))
   })
   
   output$KBgraph2 <- renderPlotly({
     plot_data <- miscelanneous_db %>%
       filter(voivodeships %in% selected_voivodeships()) %>%
       arrange(desc(elecrified))  # Sortowanie w kolejności malejącej
     
     plot_ly(plot_data, x = ~elecrified, y = ~reorder(voivodeships, elecrified), type = 'bar', orientation = 'h',
             marker = list(color = ~colors[voivodeships])) %>%
       layout(title = "Długość torów z trakcją w 2021",
              xaxis = list(title = "Długość (km)"),
              yaxis = list(title = ""),
              height = 400,  # Ustalona wysokość wykresu
              width = 900,  # Ustalona szerokość wykresu
              hovertemplate = paste('%{y}', 'posiada', '%{x}', 'km torów zelektryfikowanych, co stanowi', 
                                    '%{marker.color[0]:.1f}', '% ogółu torów zelektryfikowanych w Polsce'))  # Ustalony tekst hovera
   })
   
   output$KBgraph3 <- renderPlotly({
     plot_data <- miscelanneous_db %>%
       filter(voivodeships %in% selected_voivodeships()) %>%
       arrange(desc(unelecrified))  # Sortowanie w kolejności malejącej
     
     plot_ly(plot_data, x = ~unelecrified, y = ~reorder(voivodeships, unelecrified), type = 'bar', orientation = 'h',
             marker = list(color = ~colors[voivodeships])) %>%
       layout(title = "Długość torów bez trakcji w 2021",
              xaxis = list(title = "Długość (km)"),
              yaxis = list(title = ""),
              height = 400,  # Ustalona wysokość wykresu
              width = 900,  # Ustalona szerokość wykresu
              hovertemplate = paste('%{y}', 'posiada', '%{x}', 'km torów niezelektryfikowanych, co stanowi', 
                                    '%{marker.color[0]:.1f}', '% ogółu torów niezelektryfikowanych w Polsce'))  # Ustalony tekst hovera
   })
   
   output$KBgraph4 <- renderPlotly({
     plot_data <- miscelanneous_db %>%
       filter(voivodeships %in% selected_voivodeships()) %>%
       arrange(desc(one_rail))  # Sortowanie w kolejności malejącej
     
     plot_ly(plot_data, x = ~one_rail, y = ~reorder(voivodeships, one_rail), type = 'bar', orientation = 'h',
             marker = list(color = ~colors[voivodeships])) %>%
       layout(title = "Długość kolei jednoszynowych w 2021",
              xaxis = list(title = "Długość (km)"),
              yaxis = list(title = ""),
              height = 400,  # Ustalona wysokość wykresu
              width = 900,  # Ustalona szerokość wykresu
              hovertemplate = paste('%{y}', 'posiada', '%{x}', 'km kolei jednoszynowych, co stanowi', 
                                    '%{marker.color[0]:.1f}', '% ogółu kolei jednoszynowych w Polsce'))  # Ustalony tekst hovera
   })
   
   output$KBgraph5 <- renderPlotly({
     plot_data <- miscelanneous_db %>%
       filter(voivodeships %in% selected_voivodeships()) %>%
       arrange(desc(two_rail))  # Sortowanie w kolejności malejącej
     
     plot_ly(plot_data, x = ~two_rail, y = ~reorder(voivodeships, two_rail), type = 'bar', orientation = 'h',
             marker = list(color = ~colors[voivodeships])) %>%
       layout(title = "Długość kolei dwuszynowych w 2021",
              xaxis = list(title = "Długość (km)"),
              yaxis = list(title = ""),
              height = 400,  # Ustalona wysokość wykresu
              width = 900,  # Ustalona szerokość wykresu
              hovertemplate = paste('%{y}', 'posiada', '%{x}', 'km kolei dwuszynowych, co stanowi', 
                                    '%{marker.color[0]:.1f}', '% ogółu kolei dwuszynowych w Polsce'))  # Ustalony tekst hovera
   })
  #-----------------------------------------------------------------------------
   
   output$distPlotM2 <- renderPlot({
     df_test <- dt %>% 
       mutate(trasa = stri_split(`linie kolejowe`,regex = ', ')) 
     
     df_test2 <- dt %>% 
       filter(`nazwa stacji`== input$Miasto) %>% 
       select(`linie kolejowe`)
     napis <- as.character( df_test2[1,1])
     lista <- stri_split(napis,regex = ', ') 
     
     
     df_test$czyjest <- 0
     for (linia in lista[[1]]){
       df_test$czyjest <-df_test$czyjest + sapply(df_test$trasa, function(x) ifelse(linia %in% x ,1,0))  
     }
     df_test <- df_test %>% 
       filter(czyjest >= 1)
     nazwany <- df_test %>% 
       filter(`nazwa stacji`== input$Miasto)
     ggplot() +
       geom_polygon(data = Polska, aes(x = long, y = lat, group = group), fill="darkgrey") +
       geom_point(data = df_test, aes(x= long,y=lat, color = !!sym(input$Czynnik)),size = 2) +theme_bw()+
       labs(x = '',y='')+scale_color_gradient(low = 'yellow', high = 'red')+
       geom_point(data=nazwany,aes(x=long,y=lat),color = 'blue',size = 4)+theme_void()
     
   })
   output$textM2 <- renderText({
     df_test <- dt %>% 
       mutate(trasa = stri_split(`linie kolejowe`,regex = ',')) 
     
     df_test2 <- dt %>% 
       filter(`nazwa stacji`== input$Miasto) %>% 
       select(`linie kolejowe`)
     napis <- as.character( df_test2[1,1])
     lista <- stri_split(napis,regex = ', ') 
     paste("Liczba linii kolejowych wychodzących z danego miasta: ",length(lista[[1]]))
     
   })
   output$textM3 <- renderText({
     df_test <- dt %>% 
       mutate(trasa = stri_split(`linie kolejowe`,regex = ', ')) 
     
     df_test2 <- dt %>% 
       filter(`nazwa stacji`== input$Miasto) %>% 
       select(`linie kolejowe`)
     napis <- as.character( df_test2[1,1])
     lista <- stri_split(napis,regex = ', ') 
     
     
     df_test$czyjest <- 0
     for (linia in lista[[1]]){
       df_test$czyjest <-df_test$czyjest + sapply(df_test$trasa, function(x) ifelse(linia %in% x ,1,0))  
     }
     df_test <- df_test %>% 
       filter(czyjest >= 1)
     paste("Liczba stacji na wszystkich liniach kolejowych bezpośrednio sąsiadujących z miastem:", length(df_test$czyjest))
   })
   output$distPlotM3 <- renderPlot({
     df_test <- dt %>% 
       mutate(trasa = stri_split(`linie kolejowe`,regex = ', ')) 
     
     df_test2 <- dt %>% 
       filter(`nazwa stacji`== input$Miasto) %>% 
       select(`linie kolejowe`)
     napis <- as.character( df_test2[1,1])
     lista <- stri_split(napis,regex = ', ') 
     
     
     df_test$czyjest <- 0
     for (linia in lista[[1]]){
       df_test$czyjest <-df_test$czyjest + sapply(df_test$trasa, function(x) ifelse(linia %in% x ,1,0))  
     }
     df_test <- df_test %>% 
       filter(czyjest >= 1)
     df_test <- df_test %>% 
       arrange(-!!sym(input$Czynnik)) 
     df_test <- df_test[1:5,]
     ggplot()+
       geom_bar(data = df_test, aes(x= reorder(factor(`nazwa stacji`),-!!sym(input$Czynnik)),y = !!sym(input$Czynnik)),stat = "identity")+
       labs(title= paste("Top 5 stacji na linii ze względu na:",input$Czynnik),x="Nazwa stacji")+theme_bw()+
       scale_y_continuous(expand=c(0,0))
     
     
   })
   output$distPlotM4 <- renderPlot({
     df_test <- dt %>% 
       mutate(trasa = stri_split(`linie kolejowe`,regex = ', ')) 
     
     df_test2 <- dt %>% 
       filter(`nazwa stacji`== input$Miasto) %>% 
       select(`linie kolejowe`)
     napis <- as.character( df_test2[1,1])
     lista <- stri_split(napis,regex = ', ') 
     
     
     df_test$czyjest <- 0
     for (linia in lista[[1]]){
       df_test$czyjest <-df_test$czyjest + sapply(df_test$trasa, function(x) ifelse(linia %in% x ,1,0))  
     }
     df_test <- df_test %>% 
       filter(czyjest >= 1)
     ggplot()+
       geom_boxplot(data = df_test, aes(!!sym(input$Czynnik)))+
       labs(title= paste("Rozkład:",input$Czynnik))+theme_bw()+ theme(
         axis.title.y = element_blank(),  
         axis.text.y = element_blank(),   
         axis.ticks.y = element_blank()  
       )
     
   })
   
}



app_ui <- navbarPage(
  title = "Analiza transportu kolejowego pasażerskiego w Polsce",
  tabPanel("Na województwa", ui1),
  tabPanel("Linie kolejowe", ui2),
  tabPanel ("Opóźnienia",ui3),
  theme = bslib::bs_theme(bootswatch = "darkly"),
  header = tags$head(tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css")) )

shinyApp(app_ui, server)

