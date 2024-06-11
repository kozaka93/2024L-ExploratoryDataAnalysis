library(bsicons)
library(bslib)
library(sf)
library(ggplot2)
library(plotly)
library(ggforce)
library(dplyr)
library(ggspatial)
library(leaflet)
library(arrow)
library(readr)
library(tidyr)
library(geojson)
library(geojsonio)
library(htmltools)
library(lubridate)
library(shinythemes)
library(tigris)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)

# mapa klikana
{
styczen2023 <- read_parquet("yellow_tripdata_2023-01.parquet")


zone_lookup <- read.csv("taxi_zone_lookup.csv")

lokacje_sensowne <- zone_lookup %>% 
  filter(LocationID != 264 & LocationID != 265)

komp_wybucha <- styczen2023 %>% 
  left_join(lokacje_sensowne, by = c("PULocationID" = "LocationID")) %>% 
  rename(PUBorough = Borough, PUZone = Zone, PUservice_zone = service_zone) %>%
  left_join(lokacje_sensowne, by = c("DOLocationID" = "LocationID")) %>% 
  rename(DOBorough = Borough, DOZone = Zone, DOservice_zone = service_zone)

komp_wybucha <- komp_wybucha %>% 
  select(PULocationID, DOLocationID, PUBorough, PUZone, DOBorough, DOZone)

policzone <- komp_wybucha %>% 
  group_by(PULocationID, DOLocationID, PUBorough, PUZone, DOBorough, DOZone) %>% 
  summarise(n = n()) %>% 
  filter(PULocationID != DOLocationID) %>% 
  arrange(desc(n))

  
plik_shp <- st_read("taxi_zones.shp")

plik_shp_dwa <- plik_shp %>% 
  left_join(policzone, by = c("LocationID" = "PULocationID")) %>% 
  arrange(desc(n))


get_top_n <- function(data, group_col, value_col, top_n = 3) {
  data %>%
    group_by(across(all_of(group_col))) %>%
    arrange(desc(!!sym(value_col))) %>%
    slice_head(n = top_n) %>%
    ungroup()
}

result <- get_top_n(plik_shp_dwa, "LocationID", "n", 3)


df_grouped <- result %>%
  group_by(LocationID) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 3) %>%
  ungroup() %>%
  select(-rank)


df_extended <- df_grouped %>%
  mutate(row_id = ave(LocationID, LocationID, FUN = seq_along)) %>%
  spread(key = row_id, value = DOLocationID) %>%
  rename(koniec2 = `2`, koniec3 = `3`) %>%
  select(-LocationID)

result <- cbind(result, df_extended)


helpunio <- result %>%
  group_by(LocationID) %>%
  summarise(koniec = sum(X1, na.rm = TRUE),
            koniec2 = sum(koniec2, na.rm = TRUE),
            koniec3 = sum(koniec3, na.rm = TRUE)) %>%
  ungroup()

helpunio <- helpunio %>% 
  left_join(zone_lookup, by = "LocationID") %>% 
  select(-c(Borough, service_zone))


result1 <- result %>% 
  select(X1, n) %>% 
  filter(!is.na(X1))

result2 <- result %>% 
  select(koniec2, n) %>% 
  filter(!is.na(koniec2)) %>% 
  rename(n2 = n)


result3 <- result %>% 
  select(koniec3, n) %>% 
  filter(!is.na(koniec3))%>% 
  rename(n3 = n)


helpunio1 <- helpunio %>% 
  st_join(result1, by = c("koniec" = "X1")) %>% 
  select(c(LocationID, koniec, koniec2, koniec3, geometry, Zone, n)) %>% 
  distinct(LocationID, .keep_all = TRUE)


helpunio2 <- helpunio1 %>% 
  st_join(result2, by = "koniec2") %>% 
  rename(koniec2 = koniec2.x) %>% 
  select(c(LocationID, koniec, koniec2, koniec3, geometry, Zone, n, n2)) %>% 
  distinct(LocationID, .keep_all = TRUE)


helpunio3 <- helpunio2 %>% 
  st_join(result3, by = "koniec3") %>% 
  rename(koniec3 = koniec3.x) %>% 
  select(c(LocationID, koniec, koniec2, koniec3, geometry, Zone, n, n2, n3)) %>% 
  distinct(LocationID, .keep_all = TRUE)

result <- helpunio3


your_data_sf <- st_transform(result, crs = 4326)

your_data_sf$id <- 1:nrow(your_data_sf)

colors <- colorFactor("viridis", domain = your_data_sf$LocationID)
}

# mapa atrakcji
{
jan <- read_parquet("yellow_tripdata_2023-01.parquet")




geojson_file <- "NYC-Taxi-Zones.geojson" 
geo_data <- st_read(geojson_file)

map_zones <- leaflet(geo_data) %>%
  addTiles() %>%
  addPolygons(color = "blue", weight = 1, opacity = 1, fillOpacity = 0.5,
              highlight = highlightOptions(weight = 3, color = "red", bringToFront = TRUE),
              label = ~zone) 


zone_lookup <- read.csv("taxi_zone_lookup.csv")

lokacje_sensowne <- zone_lookup %>% 
  filter(LocationID != 264 & LocationID != 265)

jan_with_locations <- jan %>% 
  left_join(lokacje_sensowne, by = c("PULocationID" = "LocationID")) %>% 
  rename(PUBorough = Borough, PUZone = Zone, PUservice_zone = service_zone) %>%
  left_join(lokacje_sensowne, by = c("DOLocationID" = "LocationID")) %>% 
  rename(DOBorough = Borough, DOZone = Zone, DOservice_zone = service_zone)


from_count <- jan_with_locations %>%
  group_by(PULocationID) %>%
  summarise(count_from = n())

to_count <- jan_with_locations %>%
  group_by(DOLocationID) %>%
  summarise(count_to = n())


geo_data$objectid <- as.integer(geo_data$objectid)
geo_data <- left_join(geo_data, from_count, by = c("objectid" = "PULocationID"))

geo_data <- left_join(geo_data, to_count, by = c("objectid" = "DOLocationID"))




geo_data_sf <- st_as_sf(geo_data)




# Konwersja na obiekt sf

geo_data_sf <- st_as_sf(geo_data, wkt = "geometry")


value_range <- range(c(geo_data$count_from, geo_data$count_to), na.rm = TRUE)


color_func <- colorNumeric(palette = c("lightblue", "darkblue"), domain = value_range)



labels_to <- lapply(1:nrow(geo_data_sf), function(i) {
  zone_name <- geo_data_sf$zone[i]
  count_to <- geo_data_sf$count_to[i]  
  label <- paste(zone_name, ":", count_to)
  label
})


map_to <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = geo_data_sf, 
              fillColor = ~color_func(count_to),
              weight = 1,
              fillOpacity = 0.7,
              color = "white",
              label = labels_to,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
              ))%>%
  addLegend(pal = color_func, 
            values = geo_data$count_to, 
            opacity = 0.7, 
            title = "Liczba przyjazdów",
            position = "bottomright")


labels_from <- lapply(1:nrow(geo_data_sf), function(i) {
  zone_name <- geo_data_sf$zone[i]
  count_from <- geo_data_sf$count_from[i]  
  label <- paste(zone_name, ":", count_from)
  label
})


map_from <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = geo_data_sf, 
              fillColor = ~color_func(count_from),
              weight = 1,
              fillOpacity = 0.7,
              color = "white",
              label = labels_from,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
              ))%>%
  addLegend(pal = color_func, 
            values = geo_data$count_from, 
            opacity = 0.7, 
            title = "Liczba odjazdów",
            position = "bottomright")



locations <- list(
  "Broadway" = c(-73.9866, 40.7590),
  "Brooklyn Bridge" = c(-73.9969, 40.7061),
  "Central Park" = c(-73.9680, 40.7851),
  "Metropolitan Museum" = c(-73.9632, 40.7794),
  "Grand Central Terminal" = c(-73.9772, 40.7527),
  "Memorial" = c(-74.0133, 40.7115),
  "Statue of Liberty" = c(-74.0445, 40.6892),
  "Times Square" = c(-73.9857, 40.7580),
  "Empire State Building" = c(-73.9857, 40.7488),
  "Wall Street" = c(-74.0110, 40.7074)
)



createLeafletMap <- function(map_type, show_markers) {
  if (map_type == "Mapa odjazdów") {
    map <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = geo_data_sf, 
                  fillColor = ~color_func(count_from),
                  weight = 1,
                  fillOpacity = 0.7,
                  color = "white", 
                  label = ~paste(zone, ":", count_from),
                  highlightOptions = highlightOptions(
                    weight = 3,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE))
    
    if (show_markers) {
      for (loc in names(locations)) {
        map <- addMarkers(map, lng = locations[[loc]][1], lat = locations[[loc]][2], popup = loc)
      }
    }
    
    labels_from <- lapply(1:nrow(geo_data_sf), function(i) {
      zone_name <- geo_data_sf$zone[i]
      count_from <- geo_data_sf$count_from[i]
      label <- paste(zone_name, ":", count_from)
      label
    })
    
    map <- map %>%
      addLegend(pal = color_func, 
                values = geo_data$count_from, 
                opacity = 0.7, 
                title = "Liczba odjazdów ze strefy",
                position = "bottomright",
                labFormat = labelFormat(suffix = ""))
    
  } else if (map_type == "Mapa przyjazdów") {
    map <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = geo_data_sf, 
                  fillColor = ~color_func(count_to),
                  weight = 1,
                  fillOpacity = 0.7,
                  color = "white",
                  label = ~paste(zone, ":", count_from),
                  highlightOptions = highlightOptions(
                    weight = 3,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE))
    
    if (show_markers) {
      for (loc in names(locations)) {
        map <- addMarkers(map, lng = locations[[loc]][1], lat = locations[[loc]][2], popup = loc)
      }
    }
    
    labels_to <- lapply(1:nrow(geo_data_sf), function(i) {
      zone_name <- geo_data_sf$zone[i]
      count_to <- geo_data_sf$count_to[i]
      label <- paste(zone_name, ":", count_to)
      label
    })
    
    map <- map %>%
      addLegend(pal = color_func, 
                values = geo_data$count_to, 
                opacity = 0.7, 
                title = "Liczba przyjazdów do strefy",
                position = "bottomright",
                labFormat = labelFormat(suffix = ""))
  }
  
  return(map)
}

}


# rozne
{
jan_with_day <- jan %>%
  mutate(tpep_pickup_datetime = ymd_hms(tpep_pickup_datetime),
         tpep_dropoff_datetime = ymd_hms(tpep_dropoff_datetime),
         pickup_day_of_week = wday(tpep_pickup_datetime, label = TRUE),
         dropoff_day_of_week = wday(tpep_dropoff_datetime, label = TRUE))

days_count <- jan_with_day %>%
  group_by(pickup_day_of_week) %>%
  summarise(count = n())

jan_with_day <- jan_with_day %>%
  mutate(pickup_hour = hour(tpep_pickup_datetime))

hour_count <- jan_with_day %>%
  group_by(pickup_hour) %>%
  summarise(count = n())

# Przygotowanie wykresów
daily_plot <- ggplot(days_count, aes(x = pickup_day_of_week, y = count)) + 
  geom_col() + 
  labs(x = "Dzień tygodnia", y = "Liczba wezwań taksówek") +
  theme_minimal()

gg_hour <- ggplot(hour_count, aes(x = pickup_hour, y = count, 
                                  text = paste("Godzina odbioru:", pickup_hour, "<br>", "Ilość taxi:", count))) +
  geom_col(fill='#007ba7') + 
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) + 
  labs(x = "Godzina odbioru", y = "Liczba wezwań taksówek") + 
  theme(panel.grid.major.y = element_blank(),  
        panel.grid.minor = element_blank(),    
        panel.background = element_rect(fill = "white", color = NA), 
        axis.line = element_line(color = "black"),
        axis.line.y = element_blank()) 

}

# pieniadze boxploty suwakowe
{
dane_suwakowe <- jan %>%
  filter(trip_distance <= 45 & trip_distance >= 0.51)

jan_with_mean <- jan %>%
  filter(trip_distance <= 45 & trip_distance >= 0.51) %>%
  mutate(mean_for_mile = total_amount / trip_distance)

mean_plot <- jan_with_mean %>% 
  group_by(trip_distance) %>%
  summarise(sum_mean = sum(mean_for_mile),
            count = n(),
            mean_for_dist = sum_mean / count) %>%
  ggplot(aes(x = trip_distance, y = mean_for_dist)) + geom_line() +
  labs(x = "Długość podróży (w milach)", y = "Średni koszt (w dolarach)") +
  theme_minimal()

dane_suwakowe$trip_distance <- round(dane_suwakowe$trip_distance)

boxplociki <- dane_suwakowe %>%
  filter(total_amount > 0) %>%
  group_by(trip_distance) %>%
  ggplot(aes(x = trip_distance, y = total_amount, group = trip_distance)) + 
  geom_boxplot() +
  labs(x = "Długość podróży (w milach)", y = "Kwota (w dolarach)") +
  theme_minimal() + theme(axis.text = element_text(size = 20))

remove_outliers <- function(df, var) {
  Q1 <- quantile(df[[var]], 0.25)
  Q3 <- quantile(df[[var]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  df %>% filter(df[[var]] >= lower_bound & df[[var]] <= upper_bound)
}

}

# pieniądze i ciekawostki
{# również do tipów, tylko, że z lub na lotniska
  tip_from_air_cut <- read_parquet("jan_from_air.parquet")
  tip_from_air_cut <- tip_from_air_cut %>% rename(Zone=PUZone)
  tip_to_air_cut <- read_parquet("jan_to_air.parquet")
  tip_to_air_cut  <-  tip_to_air_cut %>% rename(Zone=DOZone)
  
  m <- min(length(tip_from_air_cut$Zone), length(tip_to_air_cut$Zone))
  x <- sample.int(length(tip_from_air_cut$Zone), m, replace=FALSE)
  tip_from_air_cut1 <- tip_from_air_cut %>% slice(x)
  y <- sample.int(length(tip_to_air_cut$Zone), m, replace=FALSE)
  tip_to_air_cut1 <- tip_to_air_cut %>% slice(y)
  
  # procent dlugihc podrozy z/do dzielnicy
  most_long_in <- read_parquet("most_long_in.parquet")
  most_long_in <- most_long_in %>% select(-DOBorough) %>% 
    rename(Borough=`ifelse(DOBorough == "N/A", "Outside of NYC", DOBorough)`)%>% 
    filter(Borough!="Unknown")
  
  most_long_out <- read_parquet("most_long_out.parquet")
  most_long_out <- most_long_out %>% select(-PUBorough) %>% 
    rename(Borough=`ifelse(PUBorough == "N/A", "Outside of NYC", PUBorough)`) %>% 
    filter(Borough!="Unknown")
  
  # Najszybsze dzielnice w godzinach sczytu
  rush_h <- read_parquet("rush_hour_cut2.parquet")
  
  pon <- rush_h %>% filter(PUweekday=="poniedziałek")
  wt <- rush_h %>% filter(PUweekday=="wtorek")
  sr <- rush_h %>% filter(PUweekday=="środa")
  czw <- rush_h %>% filter(PUweekday=="czwartek")
  pt <- rush_h %>% filter(PUweekday=="piątek")
  
  mi <- min(length(pon$PUBorough),length(wt$PUBorough),length(sr$PUBorough),
            length(czw$PUBorough),length(pt$PUBorough))
  x1 <- sample.int(length(pon$PUBorough), mi, replace = FALSE)
  x2 <- sample.int(length(wt$PUBorough), mi, replace = FALSE)
  x3 <- sample.int(length(sr$PUBorough), mi, replace = FALSE)
  x4 <- sample.int(length(czw$PUBorough), mi, replace = FALSE)
  x5 <- sample.int(length(pt$PUBorough), mi, replace = FALSE)
  pon <- pon %>% slice(x1)
  wt <- wt %>% slice(x2)
  sr <- sr %>% slice(x3)
  czw <- czw %>% slice(x4)
  pt <- pt %>% slice(x5)
}

ui <- fluidPage(
  
  theme = shinytheme("cerulean"),
  navbarPage(
    title = "Analiza Taksówek",
    tabPanel("Główna strona",
             fluidRow(
               column(width = 12, align = "center",
                      h1("Analiza Przejazdów Nowojorskich Taksówek", style = "font-size: 24px;"),
                      p("Celem naszego projektu jest analiza danych związanych z nowojorskimi taksówkami. Skąd dokąd najczęściej podróżuje się w Nowym Jorku? Czy strefy z których taksówki ruszały najczęściej bądź do których najczęściej dojeżdżały są w jakiś sposób powiązane z turystyką i kulturą Nowego Jorku? Ile turystę może kosztować przejazd taksówką? Na te i inne pytania znajdziemy odpowiedzi dzięki interaktywnym wykresom. ", style = "font-size: 20px;"),
                      img(src = "zdjecietax.jfif", height = "400px", width = "400px",
                          style = "display:block; margin:auto;")
               )
             )
    ),
    tabPanel("Mapy",
             fluidRow(
               column(width = 3,
                      selectInput("map_type", "Wybierz typ mapy:",
                                  choices = c("Mapa odjazdów", "Mapa przyjazdów"),
                                  selected = "Mapa odjazdów"),
                      checkboxInput("show_markers", "Pokaż lokalizacje na mapie", value = FALSE)
               ),
               column(width = 9,
                      h3("Mapa odjazdów/przyjazdów", style = "font-size: 24px;"),
                      p("Mapa pierwsza określa jak często w danej strefie pojawiały się taksówki. Jeśli dodatkowo wybierzemy dziesięć największych atrakcji Nowego Jorku, możemy zaobserwować, że największe stężenie podróży jest właśnie w ich okolicach.", style = "font-size: 20px;"),
                      leafletOutput("map", height = "400px"),
                      h3("Mapa powiązanych stref", style = "font-size: 24px;"),
                      p(" Na drugiej mapie możemy sprawdzić jakie były 3 najczęstsze miejsca destynacji dla każdej strefy. Okazuje się, że podróżujący z Manhattanu najczęściej kończą w tej samej dzielnicy. ", style = "font-size: 20px;"),
                      leafletOutput("map2", height = "400px")
               )
             )
    ),
    tabPanel("Pieniądze",
             fluidRow(
               column(width = 12,
                      h3("Analiza kosztów podróży", style = "font-size: 24px;"),
                      p("Boxplot pomaga określić podróżującym jak dużo pieniędzy mogą musieć zapłacić za przejazd taksówką w zależności od przewidywanej długości ich podróży.", style = "font-size: 20px;"),
                      sliderInput("trip_distance",
                                  "Wybierz przybliżoną długość przejazdu:",
                                  min = min(dane_suwakowe$trip_distance, na.rm = TRUE),
                                  max = max(dane_suwakowe$trip_distance, na.rm = TRUE),
                                  value = min(dane_suwakowe$trip_distance, na.rm = TRUE),
                                  step = 1)
                      )
               ),
               column(width = 12,
                      fluidRow(
                        column(width = 12,  
                               
                               plotOutput("boxplot", height = "400px")
                        )
                      ),      fluidRow(
                        column(width = 12, h3("Napiwki", style = "font-size: 24px;"),
                               p("A jak w Nowym Jorku z kulturą dawania napiwku za usługę? Ciekawą różnice pokazują podróże na 3 największe lotniska w obrębie Nowego Jorku. Okazuje się, że najmniej chętnie dają bonusy podróżujący na bądź z  lotniska w Newark.", style = "font-size: 20px;"),
                               selectInput("where", "Wybierz:",
                                                     choices = c("Z lotniska", "Na lotnisko")
                     
                        )
                      ),
                      
                      fluidRow(
                        column(width = 12,
        
                               plotOutput("violin", height = "400px")
                        )
                      )
                      
               )
             )
    ),
    tabPanel("Ciekawostki",
             fluidRow(
               column(width = 12,h3("Procent długich przejazdów", style = "font-size: 24px;"),
                      p("Wykres słupkowy idealnie potwierdza tezę z mapy - najkrótsze podróże generują manhattańczycy. Najdłużej jeżdżą ludzie ze Staten Island, czyli najbardziej oddalonego od najważniejszych punktów destynacji terenu Nowego Jorku.", style = "font-size: 20px;"),
                      selectInput("z_do", "Wybierz:",
                                  choices = c("Z dzielnicy", "Do dzielnicy"))
                      
               )),
               column(width = 12,
                      fluidRow(
                        column(width = 12,

                               plotOutput("col", height = "400px")
                        )
                      ),
                      fluidRow(
                        column(width = 12,   h3("Średnia prędkość w godzinach szczytu", style = "font-size: 24px;"),
                               p("Na prędkość przejazdu duży wpływ mają nowojorskie korki - na Manhattanie, Bronxie i Brooklynie należy uzbroić się w cierpliwość, ponieważ przejazd może trwać dłużej niż się tego oczekiwało. ", style = "font-size: 20px;"),
                               selectInput("day", "Wybierz dzień:",
                                           choices = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek"))
                        )),
                      fluidRow(
                        column(width = 12,
                               plotOutput("rush", height = "400px")
                        )
                      ),
                      fluidRow(
                        column(width = 12,
                               h3("Godziny szczytu", style = "font-size: 24px;"),
                               p("Czy nowojorskie taksówki w ogóle mają przerwy? Okazuje się, że najmniej wezwań można zaobserwować w okolicach godziny 5 nad ranem. Największego ruchu i zapotrzebowania możemy spodziewać się natomiast o godzinie 19.", style = "font-size: 20px;"),
                               plotlyOutput("hourly_plot", height = "400px")
                        )
                      )
               )
             
    )
  )
)

server <- function(input, output, session) {
  # Renderowanie wykresów
  
  output$violin <- renderPlot({ 
    z <- case_when(input$where == "Z lotniska" ~ tip_from_air_cut1,
                   input$where == "Na lotnisko" ~ tip_to_air_cut)
    v <- ifelse(input$where == "Z lotniska", "z danego lotniska", "na dane lotnisko")
    xx <- paste("Rozkład wysokości napiwków w przejazdach ", v, sep = "") 
    
    violin <- ggplot(z, aes(tip_amount)) +
      geom_histogram(bins = 12, fill='#007ba7') +
      facet_wrap(~Zone, scales = "free_y") +
      scale_y_continuous(expand=c(0,0))+
      theme_bw() +
      labs(x = "Wysokość napiwka", y = "Liczba napiwków", title = xx)+
      theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16),
            plot.title = element_text(size = 20))
    violin
  })
  
  output$col <- renderPlot({
    z <- case_when(input$z_do == "Z dzielnicy" ~ most_long_out,
                   input$z_do == "Do dzielnicy" ~ most_long_in)
    v <- ifelse(input$z_do == "Z dzielnicy", "z", "do")
    xx <- paste("Procent długich przejazdów we wszystkich przejazdach ", v, " danej dzielnicy", sep = "")
    col <- ggplot(z, aes(x = Borough, y = mean)) +
      geom_col(fill='#007ba7') +
      theme_bw() +
      labs(x = "Dzielnica", y = "Frakcja", title = xx)+
       scale_y_continuous(expand = c(0,0))+
      theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16),
            plot.title = element_text(size = 20))
    col
  })
  
  output$rush <- renderPlot({
    z <- case_when(input$day == "poniedziałek" ~ pon,
                   input$day == "wtorek" ~ wt,
                   input$day == "środa" ~ sr,
                   input$day == "czwartek" ~ czw,
                   input$day == "piątek" ~ pt)
    rush <- ggplot(z, aes(x = PUBorough, y = mph)) +
      geom_boxplot() +
      theme_bw() +
      labs(x = "Dzielnica", y = "Średnia prędkość", title = "Rozkład średniej prędkości przejazdu w godzinach szczytu w danych dzielnicach")+
      theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16),
            plot.title = element_text(size = 20))
    rush
  })
  
  output$hourly_plot <- renderPlotly({
    ggplotly(gg_hour, tooltip = "text")
  })
  
  filtered_data <- reactive({
    data <- dane_suwakowe %>%
      filter(total_amount > 0, trip_distance == input$trip_distance)
    remove_outliers(data, "total_amount")
  })
  
  output$boxplot <- renderPlot({
    filtered_data() %>%
      ggplot(aes(x = factor(trip_distance), y = total_amount)) +
      geom_boxplot() +
      labs(x = "Długość podróży (w milach)", y = "Kwota (w dolarach)") +
      theme_minimal()+theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16),
                            plot.title = element_text(size = 20))
  })
  
  output$map <- renderLeaflet({
    map <- createLeafletMap(input$map_type, input$show_markers)
    map
  })
  
  output$map2 <- renderLeaflet({
    leaflet(data = your_data_sf) %>%
      addTiles() %>%
      addPolygons(
        fillColor = "transparent",
        weight = 1,
        opacity = 1,
        color = '#636363',
        fillOpacity = 0.7,
        layerId = ~LocationID,
        highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.7, bringToFront = TRUE),
        label = ~sprintf("Strefa: %s (ID: %s)", as.character(Zone), as.character(LocationID)),
        group = "zones"
      )
  })
  
  observeEvent(input$map2_shape_click, {
    click_id <- input$map2_shape_click$id
    if (!is.null(click_id) && click_id %in% your_data_sf$LocationID) {
      selected_row <- your_data_sf[your_data_sf$LocationID == click_id, ]
      
      related_ids <- c(
        selected_row$koniec,
        selected_row$koniec2,
        selected_row$koniec3
      )
      
      related_rows <- your_data_sf[your_data_sf$LocationID %in% related_ids, ]
      
      leafletProxy("map2") %>%
        clearGroup(group = "highlight_polygon") %>%
        addPolygons(
          data = selected_row,
          fillColor = "darkred",
          weight = 1,
          opacity = 1,
          color = "#636363",
          fillOpacity = 0.7,
          layerId = ~LocationID,
          group = "highlight_polygon",
          label = ~sprintf("Strefa: %s (ID: %s)", as.character(Zone), as.character(LocationID)),
          labelOptions = labelOptions(style = list("color" = "black"), textsize = "15px", direction = "auto")
        ) %>%
        addPolygons(
          data = related_rows,
          fillColor = "#007ba7",
          weight = 1,
          opacity = 1,
          color = "#636363",
          fillOpacity = 0.7,
          layerId = ~LocationID,
          group = "highlight_polygon",
          label = ~sprintf("Strefa: %s (ID: %s), Podróże: %s",
                           as.character(Zone), as.character(LocationID),
                           ifelse(LocationID == selected_row$koniec, selected_row$n,
                                  ifelse(LocationID == selected_row$koniec2, selected_row$n2, selected_row$n3))),
          labelOptions = labelOptions(style = list("color" = "black"), textsize = "15px", direction = "auto")
        )
    }
  })
}

shinyApp(ui = ui, server = server)





