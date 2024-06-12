library(leaflet)
library(shiny)
library(dplyr)
library(tidyr)
library(data.table)
library(RColorBrewer)
library(DescTools)
library(ggplot2)
library(sf)
library(plotly)

#### Wczytywanie danych ####
routes <- read.table("E:/Dokumenty/Studia PW/materiały/Semestr IV/Wstęp do eksporacji/Ćwiczenia/Projekt 2/warsaw/routes.txt",header = TRUE,sep = ",", )
shapes <- read.table("E:/Dokumenty/Studia PW/materiały/Semestr IV/Wstęp do eksporacji/Ćwiczenia/Projekt 2/warsaw/shapes.txt",header = TRUE,sep = ",", )
trips <- read.table("E:/Dokumenty/Studia PW/materiały/Semestr IV/Wstęp do eksporacji/Ćwiczenia/Projekt 2/warsaw/trips.txt",header = TRUE,sep = ",",)
stops <- read.table("E:/Dokumenty/Studia PW/materiały/Semestr IV/Wstęp do eksporacji/Ćwiczenia/Projekt 2/warsaw/stops.txt",header = TRUE,sep = ",",)
stops_times <- read.table("E:/Dokumenty/Studia PW/materiały/Semestr IV/Wstęp do eksporacji/Ćwiczenia/Projekt 2/warsaw/stop_times.txt",header = TRUE,sep = ",",)

##########################################################
####### Mapa ze średnim czasm oczekiwania (Kuba) #########

#Zamiana na data.table, żeby było szybciej
stops_times_dat <- as.data.table(stops_times)
stops_times_sep <- stops_times_dat[, c("date","route_id", "route_variant", "day", "starting_hour") := tstrsplit(trip_id, split = "/", fill = TRUE)]


        #### Średni czas oczekiwania w dzień powszedni autobusy ####

#Wybierania rokładów z dnia 1 maja i pomijanie metra
stops_times_without_metro <- stops_times_sep[date == "RA240509"]

#Wybieranie tylko weekendów i dni powszednich w dzień
stops_times_without_metro_day <- stops_times_without_metro[day %in%  c("DP")]
stops_times_without_metro_day_aut_Z <- stops_times_without_metro_day[route_id %in% c("Z12","Z17","ZA1","ZS2","Z28","Z31")]
stops_times_without_metro_day_aut_nr <- stops_times_without_metro_day[, is_numeric := grepl("^[0-9]+$", route_id)]

# Ustawienie wartości 0 dla nieliczbowych wierszy
stops_times_without_metro_day_aut_nr[is_numeric == FALSE, route_id := 0]

#Przekztałcanie route_id na numeric

stops_times_without_metro_day_aut_nr[,route_id := as.integer(route_id)]

# Filtracja wierszy, gdzie mamy tylko autobusy oznaczone liczbami od 100 do 900
stops_times_without_metro_day_aut_numery <- stops_times_without_metro_day_aut_nr[100 <= route_id & route_id <= 900][,is_numeric := NULL]

#Łaczenie wszystkich autobusów
stops_times_without_metro_day_aut = rbind(stops_times_without_metro_day_aut_numery,stops_times_without_metro_day_aut_Z)


#Wybieranie nocnych rozkładów jazdy
#stops_times_without_metro_night <- stops_times_without_metro[day %in% c("NS","NP","NO")]


bus <- stops_times_without_metro_day_aut %>%
  mutate(  #Zamieniamy czas na odpowiedni typ
    hours = as.numeric(substring(arrival_time,1,2)),
    minutes = as.numeric(substring(arrival_time,4,5)),
    time = hours + minutes/60,
  ) %>%
  #Usuwamy ewentualne NaN i NA
  filter(!(is.na(time) | is.nan(time))) %>%
  #Liczymy średni czas
  arrange(stop_id, time) %>%
  group_by(stop_id) %>%
  summarise(avarage_time = mean(Winsorize(diff(time), probs = c(0.05,0.95)) )) %>%
  left_join(stops, by = "stop_id") %>%
  mutate(avarage_time = round(avarage_time*60)) %>%
  filter(avarage_time < 120 & avarage_time > 0) %>%
  filter(!(is.na(avarage_time) | is.nan(avarage_time))) %>%
  mutate(popup_text = paste0(stop_name, "<br> Avargae time between arrivals is ", as.character(avarage_time), " minutes")) %>%
  mutate(inverse_avarage_time = 1/avarage_time)


#### Średni czas oczekiwania w dzien powszedni tramwaje ####

#Wybierania rokładów z dnia 1 maja i pomijanie metra
stops_times_without_metro <- stops_times_sep[date == "RA240509"]

#Wybieranie tylko weekendów i dni powszednich w dzień
stops_times_without_metro_day <- stops_times_without_metro[day %in%  c("DP")]
stops_times_without_metro_day_tram_nr <- stops_times_without_metro_day[, is_numeric := grepl("^[0-9]+$", route_id)]

# Ustawienie wartości 0 dla nieliczbowych wierszy
stops_times_without_metro_day_tram_nr[is_numeric == FALSE, route_id := 0]

#Przekztałcanie route_id na numeric

stops_times_without_metro_day_tram_nr[,route_id := as.integer(route_id)]

# Filtracja wierszy, gdzie mamy tylko tramwaje oznaczone liczbami od 1 do 79
stops_times_without_metro_day_tram <- stops_times_without_metro_day_tram_nr[100 > route_id & route_id > 0][,is_numeric := NULL]


tram <- stops_times_without_metro_day_tram %>%
  mutate(  #Zamieniamy czas na odpowiedni typ
    hours = as.numeric(substring(arrival_time,1,2)),
    minutes = as.numeric(substring(arrival_time,4,5)),
    time = hours + minutes/60,
  ) %>%
  #Usuwamy ewentualne NaN i NA
  filter(!(is.na(time) | is.nan(time))) %>%
  #Liczymy średni czas
  arrange(stop_id, time) %>%
  group_by(stop_id) %>%
  summarise(avarage_time = mean(Winsorize(diff(time), probs = c(0.05,0.95)) )) %>%
  left_join(stops, by = "stop_id") %>%
  mutate(avarage_time = round(avarage_time*60)) %>%
  filter(avarage_time < 60) %>%
  filter(!(is.na(avarage_time) | is.nan(avarage_time))) %>%
  mutate(popup_text = paste0(stop_name, "<br> Avargae time between arrivals is ", as.character(avarage_time), " minutes")) %>%
  mutate(inverse_avarage_time = 1/avarage_time)

#### Średni czas oczekiwania w dzień powszedni SKM ####

#Wybierania rokładów z dnia 1 maja i pomijanie metra
stops_times_without_metro <- stops_times_sep[date == "RA240509"]

#Wybieranie tylko weekendów i dni powszednich w dzień
stops_times_without_metro_day <- stops_times_without_metro[day %in%  c("DP")]
stops_times_without_metro_day_skm <- stops_times_without_metro_day[route_id %in% c("S1","S2","S3","S10","S40","S4","S20")]



SKM <- stops_times_without_metro_day_skm %>%
  mutate(  #Zamieniamy czas na odpowiedni typ
    hours = as.numeric(substring(arrival_time,1,2)),
    minutes = as.numeric(substring(arrival_time,4,5)),
    time = hours + minutes/60,
  ) %>%
  #Usuwamy ewentualne NaN i NA
  filter(!(is.na(time) | is.nan(time))) %>%
  #Liczymy średni czas
  arrange(stop_id, time) %>%
  group_by(stop_id) %>%
  summarise(avarage_time = mean(Winsorize(diff(time), probs = c(0.05,0.95)) )) %>%
  left_join(stops, by = "stop_id") %>%
  mutate(avarage_time = round(avarage_time*60)) %>%
  filter(avarage_time < 60 & avarage_time >0) %>%
  filter(!(is.na(avarage_time) | is.nan(avarage_time))) %>%
  mutate(popup_text = paste0(stop_name, "<br> Avargae time between arrivals is ", as.character(avarage_time), " minutes")) %>%
  mutate(inverse_avarage_time = 1/avarage_time)

##### Tworzenie tabelki weekdays #####

weekdays <- list(bus,tram, SKM)

#### Średni czas oczekiwania w weekend autobusy ####


#Wybieranie tylko weekendów i dni powszednich w dzień
stops_times_without_metro_sat <- stops_times_without_metro[day %in%  c("SB")]
stops_times_without_metro_sun <- stops_times_without_metro[day %in%  c("DS")]

stops_times_without_metro_day_aut_sat_Z <- stops_times_without_metro_sat[route_id %in% c("Z12","Z17","ZA1","ZS2","Z28","Z31")]
stops_times_without_metro_day_aut_sat_nr <- stops_times_without_metro_sat[, is_numeric := grepl("^[0-9]+$", route_id)]

stops_times_without_metro_day_aut_sun_Z <- stops_times_without_metro_sun[route_id %in% c("Z12","Z17","ZA1","ZS2","Z28","Z31")]
stops_times_without_metro_day_aut_sun_nr <- stops_times_without_metro_sun[, is_numeric := grepl("^[0-9]+$", route_id)]


# Ustawienie wartości 0 dla nieliczbowych wierszy
stops_times_without_metro_day_aut_sat_nr[is_numeric == FALSE, route_id := 0]
stops_times_without_metro_day_aut_sun_nr[is_numeric == FALSE, route_id := 0]

#Przekztałcanie route_id na numeric

stops_times_without_metro_day_aut_sat_nr[,route_id := as.integer(route_id)]
stops_times_without_metro_day_aut_sun_nr[,route_id := as.integer(route_id)]

# Filtracja wierszy, gdzie mamy tylko autobusy oznaczone liczbami od 100 do 900
stops_times_without_metro_day_aut_numery_sat <- stops_times_without_metro_day_aut_sat_nr[100 <= route_id & route_id <= 900][,is_numeric := NULL]
stops_times_without_metro_day_aut_numery_sun <- stops_times_without_metro_day_aut_sun_nr[100 <= route_id & route_id <= 900][,is_numeric := NULL]

#Łaczenie wszystkich autobusów
stops_times_without_metro_day_aut_sat = rbind(stops_times_without_metro_day_aut_numery_sat,stops_times_without_metro_day_aut_sat_Z)
stops_times_without_metro_day_aut_sun = rbind(stops_times_without_metro_day_aut_numery_sun,stops_times_without_metro_day_aut_sun_Z)

#Obliczanie średniego czasu jazdy w sobotę

autobusy_sat <- stops_times_without_metro_day_aut_sat %>%
  mutate(  #Zamieniamy czas na odpowiedni typ
    hours = as.numeric(substring(arrival_time,1,2)),
    minutes = as.numeric(substring(arrival_time,4,5)),
    time = hours + minutes/60,
  ) %>%
  #Usuwamy ewentualne NaN i NA
  filter(!(is.na(time) | is.nan(time))) %>%
  #Liczymy średni czas
  arrange(stop_id, time) %>%
  group_by(stop_id) %>%
  summarise(avarage_time = mean(Winsorize(diff(time), probs = c(0.05,0.95)) )) %>%
  left_join(stops, by = "stop_id") %>%
  filter(avarage_time < 120 & avarage_time > 0) %>%
  filter(!(is.na(avarage_time) | is.nan(avarage_time))) %>%
  mutate(inverse_avarage_time = 1/avarage_time)

#Obliczanie średniego czasu jazdy w niedzielę

autobusy_sun <- stops_times_without_metro_day_aut_sun %>%
  mutate(  #Zamieniamy czas na odpowiedni typ
    hours = as.numeric(substring(arrival_time,1,2)),
    minutes = as.numeric(substring(arrival_time,4,5)),
    time = hours + minutes/60,
  ) %>%
  #Usuwamy ewentualne NaN i NA
  filter(!(is.na(time) | is.nan(time))) %>%
  #Liczymy średni czas
  arrange(stop_id, time) %>%
  group_by(stop_id) %>%
  summarise(avarage_time = mean(Winsorize(diff(time), probs = c(0.05,0.95)) )) %>%
  left_join(stops, by = "stop_id") %>%
  filter(avarage_time < 120 & avarage_time > 0) %>%
  filter(!(is.na(avarage_time) | is.nan(avarage_time))) %>%
  mutate(inverse_avarage_time = 1/avarage_time)

bus <- autobusy_sun %>%
  left_join(autobusy_sat, by = "stop_id") %>%
  mutate(avarage_time_y = case_when(is.na(avarage_time.y) ~ avarage_time.x, TRUE ~ avarage_time.y )) %>%
  mutate(avarage_time = rowMeans(cbind(avarage_time_y, avarage_time.x))) %>%
  rename(stop_name = stop_name.x) %>%
  rename(stop_lon = stop_lon.x, stop_lat = stop_lat.x) %>%
  mutate(avarage_time = round(avarage_time*60)) %>%
  mutate(popup_text = paste0(stop_name, "<br> Avargae time between arrivals is ", as.character(avarage_time), " minutes")) %>%
  mutate(inverse_avarage_time = 1/avarage_time)
  
  


#### Średni czas oczekiwania w weekend tramwaje ####

stops_times_without_metro_sat <- stops_times_without_metro[day %in%  c("SB")]
stops_times_without_metro_sun <- stops_times_without_metro[day %in%  c("DS")]

stops_times_without_metro_day_tram_nr_sat <- stops_times_without_metro_sat[, is_numeric := grepl("^[0-9]+$", route_id)]
stops_times_without_metro_day_tram_nr_sun <- stops_times_without_metro_sun[, is_numeric := grepl("^[0-9]+$", route_id)]


# Ustawienie wartości 0 dla nieliczbowych wierszy
stops_times_without_metro_day_tram_nr_sat[is_numeric == FALSE, route_id := 0]
stops_times_without_metro_day_tram_nr_sun[is_numeric == FALSE, route_id := 0]


#Przekztałcanie route_id na numeric

stops_times_without_metro_day_tram_nr_sat[,route_id := as.integer(route_id)]
stops_times_without_metro_day_tram_nr_sun[,route_id := as.integer(route_id)]

# Filtracja wierszy, gdzie mamy tylko tramwaje oznaczone liczbami od 1 do 79
stops_times_without_metro_day_tram_sat <- stops_times_without_metro_day_tram_nr_sat[100 > route_id & route_id > 0][,is_numeric := NULL]
stops_times_without_metro_day_tram_sun <- stops_times_without_metro_day_tram_nr_sun[100 > route_id & route_id > 0][,is_numeric := NULL]


tramwaje_sat <- stops_times_without_metro_day_tram_sat %>%
  mutate(  #Zamieniamy czas na odpowiedni typ
    hours = as.numeric(substring(arrival_time,1,2)),
    minutes = as.numeric(substring(arrival_time,4,5)),
    time = hours + minutes/60,
  ) %>%
  #Usuwamy ewentualne NaN i NA
  filter(!(is.na(time) | is.nan(time))) %>%
  #Liczymy średni czas
  arrange(stop_id, time) %>%
  group_by(stop_id) %>%
  summarise(avarage_time = mean(Winsorize(diff(time), probs = c(0.05,0.95)) )) %>%
  left_join(stops, by = "stop_id") %>%
  filter(avarage_time < 60) %>%
  filter(!(is.na(avarage_time) | is.nan(avarage_time)))


tramwaje_sun <- stops_times_without_metro_day_tram_sun %>%
  mutate(  #Zamieniamy czas na odpowiedni typ
    hours = as.numeric(substring(arrival_time,1,2)),
    minutes = as.numeric(substring(arrival_time,4,5)),
    time = hours + minutes/60,
  ) %>%
  #Usuwamy ewentualne NaN i NA
  filter(!(is.na(time) | is.nan(time))) %>%
  #Liczymy średni czas
  arrange(stop_id, time) %>%
  group_by(stop_id) %>%
  summarise(avarage_time = mean(Winsorize(diff(time), probs = c(0.05,0.95)) )) %>%
  left_join(stops, by = "stop_id") %>%
  filter(avarage_time < 60) %>%
  filter(!(is.na(avarage_time) | is.nan(avarage_time)))


tram <- tramwaje_sun %>%
  left_join(tramwaje_sat, by = "stop_id") %>%
  mutate(avarage_time = rowMeans(cbind(avarage_time.y, avarage_time.x))) %>%
  rename(stop_name = stop_name.x) %>%
  rename(stop_lon = stop_lon.x, stop_lat = stop_lat.x) %>%
  mutate(avarage_time = round(avarage_time*60)) %>%
  mutate(popup_text = paste0(stop_name, "<br> Avargae time between arrivals is ", as.character(avarage_time), " minutes")) %>%
  mutate(inverse_avarage_time = 1/avarage_time)

#### Średni czas oczekiwania w weekedn SKM ####

#Wybieranie tylko weekendów i dni powszednich w dzień
stops_times_without_metro_day_sat <- stops_times_without_metro[day %in%  c("SB")]
stops_times_without_metro_day_sun <- stops_times_without_metro[day %in%  c("DS")]

stops_times_without_metro_day_skm_sat <- stops_times_without_metro_day_sat[route_id %in% c("S1","S2","S3","S10","S40","S4","S20")] #Wychodzi zero recordów - usuwamy
stops_times_without_metro_day_skm_sun <- stops_times_without_metro_day_sun[route_id %in% c("S1","S2","S3","S10","S40","S4","S20")]

SKM <- stops_times_without_metro_day_skm_sun %>%
  mutate(  #Zamieniamy czas na odpowiedni typ
    hours = as.numeric(substring(arrival_time,1,2)),
    minutes = as.numeric(substring(arrival_time,4,5)),
    time = hours + minutes/60,
  ) %>%
  #Usuwamy ewentualne NaN i NA
  filter(!(is.na(time) | is.nan(time))) %>%
  #Liczymy średni czas
  arrange(stop_id, time) %>%
  group_by(stop_id) %>%
  summarise(avarage_time = mean(Winsorize(diff(time), probs = c(0.05,0.95)) )) %>%
  left_join(stops, by = "stop_id") %>%
  mutate(avarage_time = round(avarage_time*60)) %>%
  filter(avarage_time < 60 & avarage_time >0) %>%
  filter(!(is.na(avarage_time) | is.nan(avarage_time))) %>%
  mutate(popup_text = paste0(stop_name, "<br> Średni czas oczekiwania wynosi ", as.character(avarage_time), " minut")) %>%
  mutate(inverse_avarage_time = 1/avarage_time)

##### Tworzenie tabelki weekends&holidys #####

weekendsAndHolidays <- list(bus,tram,SKM)



##############################################################################################
####### 1 % najczęściej odwiedzanych przystanków oraz dostępność komunikacyjn w gminach (Mateusz) ######
# Przekonwertuj na obiekt sf z właściwym CRS (WGS 84)
stops_sf <- st_as_sf(stops, coords = c("stop_lon", "stop_lat"), crs = 4326)

# Obliczenie liczby przyjazdów na każdy przystanek
przystanki_liczba_przyjazdow <- stops_times %>%
  group_by(stop_id) %>%
  summarise(liczba_przyjazdow = n())

# Obliczenie liczby unikalnych linii (trip_id) na każdy przystanek
przystanki_liczba_linii <- stops_times %>%
  left_join(trips, by = "trip_id") %>%
  group_by(stop_id) %>%
  summarise(liczba_linii = n_distinct(route_id))

# Połączenie danych przystanków z liczbą przyjazdów i liczbą linii
stops_with_info <- stops_sf %>%
  left_join(przystanki_liczba_przyjazdow, by = "stop_id") %>%
  left_join(przystanki_liczba_linii, by = "stop_id") %>%
  filter(!is.na(liczba_przyjazdow) & !is.na(liczba_linii))

# Znalezienie progu 1% najpopularniejszych przystanków pod względem liczby przyjazdów
prog_przyjazdy <- quantile(stops_with_info$liczba_przyjazdow, 0.99)

# Filtrujemy przystanki do 1% najpopularniejszych
top_stops <- stops_with_info %>%
  filter(liczba_przyjazdow >= prog_przyjazdy)

#############################################
####### Najlepiej skomunikowane gminy #######

granice_path <- "E:/Dokumenty/Studia PW/materiały/Semestr IV/Wstęp do eksporacji/Ćwiczenia/Projekt 2/gminy"
# Przekonwertuj na obiekt sf z właściwym CRS (WGS 84)
stops_sf <- st_as_sf(stops, coords = c("stop_lon", "stop_lat"), crs = 4326)

# Wczytanie plików shapefile z granicami administracyjnymi
shapefiles <- list.files(granice_path, pattern = "\\.shp$", full.names = TRUE)
admin_boundaries_path <- shapefiles[1]
admin_boundaries <- st_read(admin_boundaries_path)

# Naprawa geometrii granic
admin_boundaries <- st_make_valid(admin_boundaries)

# Przekształcenie CRS granic w razie potrzeby
if (st_crs(stops_sf) != st_crs(admin_boundaries)) {
  admin_boundaries <- st_transform(admin_boundaries, st_crs(stops_sf))
}

# Załaduj dane o kursach dla każdego przystanku
stop_times <- stops_times

# Konwersja czasów na format POSIXct
stop_times <- stop_times %>%
  mutate(
    arrival_time = as.POSIXct(arrival_time, format = "%H:%M:%S", tz = "UTC"),
    departure_time = as.POSIXct(departure_time, format = "%H:%M:%S", tz = "UTC")
  )

# Obliczenie różnic czasów przyjazdu między kolejnymi autobusami na danym przystanku
stop_times <- stop_times %>%
  arrange(stop_id, arrival_time) %>%
  group_by(stop_id) %>%
  mutate(wait_time = as.numeric(difftime(lead(arrival_time), arrival_time, units = "secs"))) %>%
  filter(!is.na(wait_time) & wait_time > 0)

# Obliczenie średnich czasów oczekiwania na przystanku
average_wait_times <- stop_times %>%
  group_by(stop_id) %>%
  summarise(average_wait_time = mean(wait_time, na.rm = TRUE))

# Połączenie danych przystanków ze średnimi czasami oczekiwania
stops_with_wait_times <- stops_sf %>%
  left_join(average_wait_times, by = "stop_id") %>%
  filter(!is.na(average_wait_time))

# Dopasowanie przystanków do jednostek administracyjnych
stops_with_admin <- st_join(stops_with_wait_times, admin_boundaries, join = st_within)

# Filtracja przystanków poza Warszawą
przystanki_poza_warszawa <- stops_with_admin %>%
  filter(is.na(JPT_NAZWA_) | !grepl("Warszawa", JPT_NAZWA_, ignore.case = TRUE))

# Obliczenie średniego czasu oczekiwania dla każdej miejscowości
miejscowosci_czas_oczekiwania <- przystanki_poza_warszawa %>%
  group_by(JPT_NAZWA_) %>%
  summarise(sredni_czas_oczekiwania = mean(average_wait_time, na.rm = TRUE) / 60) %>%
  arrange(sredni_czas_oczekiwania) %>%
  select(miejscowosc = JPT_NAZWA_, sredni_czas_oczekiwania)

########################################
####### Wykresy słupkowe (Ala) #########

df1 <- stops_times %>% 
  group_by(trip_id) %>% 
  summarise(n_of_stops = max(stop_sequence) + 1,
            distance = max(shape_dist_traveled))

df2 <- df1 %>% 
  left_join(trips, by = "trip_id")

df2 <- df2 %>% 
  select(route_id, trip_headsign, n_of_stops, distance)

df3 <- df2 %>% 
  group_by(route_id) %>% 
  summarise(n_of_stops = max(n_of_stops),
            distance = max(distance))

df3$distance[df3$route_id == "M1"] <- 23.1
df3$distance[df3$route_id == "M2"] <- 19

df3$dist_between_st <- df3$distance / df3$n_of_stops

df3 <- df3 %>% 
  mutate(category = case_when(
    route_id %in% 1:99 ~ "tram",
    route_id %in% 100:299 ~ "regular line",
    route_id %in% 300:399 ~ "regular periodic line",
    route_id %in% 400:499 ~ "accelerated periodic line",
    route_id %in% 500:599 ~ "accelerated line",
    route_id %in% 700:799 ~ "regular zone line",
    route_id %in% 800:899 ~ "periodic zone line",
    route_id %in% 900:999 ~ "special and additional line",
    startsWith(route_id, "N") ~ "night line",
    startsWith(route_id, "E") ~ "periodic express line",
    startsWith(route_id, "L") ~ "local suburban line supplementary",
    startsWith(route_id, "Z") ~ "replacement line",
    startsWith(route_id, "S") ~ "train",
    .default = "subway"
  ))

df3 %>% 
  arrange(- n_of_stops) %>% 
  filter(category == "regular line") %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(route_id, n_of_stops), y = n_of_stops)) +
  geom_bar(stat = "identity", position = "dodge", fill = "red") +
  labs(title = "TOP10 regular lines with the largest number of stops",
       x = "Line number",
       y = "Number of stops")

df3 %>% 
  arrange(- n_of_stops) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(route_id, n_of_stops), y = n_of_stops)) +
  geom_bar(stat = "identity", position = "dodge", fill = "red") +
  labs(title = "TOP10 lines with the largest number of stops",
       x = "Line number",
       y = "Number of stops")

df3 %>% 
  arrange(- dist_between_st) %>% 
  filter(category == "regular line") %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(route_id, dist_between_st), y = dist_between_st)) +
  geom_bar(stat = "identity", position = "dodge", fill = "yellow3") +
  labs(title = "TOP10 regular lines with the longest average distance between stops",
       x = "Line number",
       y = "Average distance between stops")

df3 %>% 
  arrange(dist_between_st) %>% 
  filter(category == "regular line") %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(route_id, dist_between_st), y = dist_between_st)) +
  geom_bar(stat = "identity", position = "dodge", fill = "yellow3") +
  labs(title = "TOP10 regular lines with the shortest average distance between stops",
       x = "Line number",
       y = "Average distance between stops")

df3 %>% 
  arrange(- dist_between_st) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(route_id, dist_between_st), y = dist_between_st)) +
  geom_bar(stat = "identity", position = "dodge", fill = "yellow3") +
  labs(title = "TOP10 lines with the longest average distance between stops",
       x = "Line number",
       y = "Average distance between stops")

df3 %>% 
  arrange(dist_between_st) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(route_id, dist_between_st), y = dist_between_st)) +
  geom_bar(stat = "identity", position = "dodge", fill = "yellow3") +
  labs(title = "TOP10 lines with the shortest average distance between stops",
       x = "Line number",
       y = "Average distance between stops")


###################################
####### Tworzenie aplikacji #######

server <- function(input,output,session) {
  output$map1 <- renderLeaflet({
    selected_data <- get(input$days)
    transport_type <- as.numeric(input$transport)
    
    selected_data <- selected_data[[transport_type]]
    
    palette <- colorNumeric(palette = "Reds", domain = selected_data$inverse_avarage_time)
    
    map <- leaflet(data = selected_data) %>%
      setView(lng = 21.0122, lat = 52.2297, zoom = 12) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~ stop_lon, lat = ~ stop_lat, popup = ~ popup_text, 
                       fillColor = ~ palette(inverse_avarage_time),
                       radius = 7, stroke = TRUE, weight = 2 ,color = "black",
                       fillOpacity = 0.9, opacity = 1) %>%
      addLegend(
        "bottomright",
        pal = palette,
        values = ~inverse_avarage_time,
        labFormat = labelFormat(
          transform = function(x) round(1 / x,1) 
        ),
        opacity = 1,
        title = "Avarage time (minutes)"
      )
  })
  
  data_reactive <- reactive({
    chosen_types <- input$type_of_transport
    data_all <- data.frame(type = character(), n_of_stops = numeric(), dist_between_st = numeric(), route_id = character(), stringsAsFactors = FALSE)
    
    if (1 %in% chosen_types) {
      data_all <- rbind(data_all, df3[df3$category == "tram", ])
    }
    
    if (2 %in% chosen_types) {
      data_all <- rbind(data_all, df3[df3$category == "regular line", ])
    }
    
    if (3 %in% chosen_types) {
      data_all <- rbind(data_all, df3[df3$category == "regular periodic line", ])
    }
    
    if (4 %in% chosen_types) {
      data_all <- rbind(data_all, df3[df3$category == "accelerated periodic line", ])
    }
    
    if (5 %in% chosen_types) {
      data_all <- rbind(data_all, df3[df3$category == "accelerated line", ])
    }
    
    if (6 %in% chosen_types) {
      data_all <- rbind(data_all, df3[df3$category == "regular zone line", ])
    }
    
    if (7 %in% chosen_types) {
      data_all <- rbind(data_all, df3[df3$category == "periodic zone line", ])
    }
    
    if (8 %in% chosen_types) {
      data_all <- rbind(data_all, df3[df3$category == "special and additional line", ])
    }
    
    if (9 %in% chosen_types) {
      data_all <- rbind(data_all, df3[df3$category == "night line", ])
    }
    
    if (10 %in% chosen_types) {
      data_all <- rbind(data_all, df3[df3$category == "periodic express line", ])
    }
    
    if (11 %in% chosen_types) {
      data_all <- rbind(data_all, df3[df3$category == "local suburban line supplementary", ])
    }
    
    if (12 %in% chosen_types) {
      data_all <- rbind(data_all, df3[df3$category == "replacement line", ])
    }
    
    if (13 %in% chosen_types) {
      data_all <- rbind(data_all, df3[df3$category == "train", ])
    }
    
    if (14 %in% chosen_types) {
      data_all <- rbind(data_all, df3[df3$category == "subway", ])
    }
    
    data_1 <- data_all %>%
      arrange(-n_of_stops) %>% 
      head(10)
    
    data_2 <- data_all %>%
      arrange(-dist_between_st) %>% 
      head(10)
    
    data_3 <- data_all %>%
      arrange(n_of_stops) %>% 
      head(10)
    
    data_4 <- data_all %>%
      arrange(dist_between_st) %>% 
      head(10)
    
    list(
      data_1 = data_1,
      data_2 = data_2,
      data_3 = data_3,
      data_4 = data_4
    )
  })
  
  
  output$plot_1 <- renderPlot({
    data_plot_1 <- data_reactive()$data_1
    
    ggplot(data_plot_1, aes(x = reorder(route_id, n_of_stops), y = n_of_stops)) +
      geom_bar(stat = "identity", position = "dodge", fill = "red") +
      labs(title = "TOP lines with the largest number of stops",
           x = "Line number",
           y = "Number of stops")
  })
  
  output$plot_2 <- renderPlot({
    data_plot_2 <- data_reactive()$data_2
    
    ggplot(data_plot_2, aes(x = reorder(route_id, dist_between_st), y = dist_between_st)) +
      geom_bar(stat = "identity", position = "dodge", fill = "yellow3") +
      labs(title = "TOP lines with the longest average distance between stops",
           x = "Line number",
           y = "Average distance between stops")
  })
  
  output$plot_3 <- renderPlot({
    data_plot_3 <- data_reactive()$data_3
    
    ggplot(data_plot_3, aes(x = reorder(route_id, -n_of_stops), y = n_of_stops)) +
      geom_bar(stat = "identity", position = "dodge", fill = "red") +
      labs(title = "TOP lines with the smallest number of stops",
           x = "Line number",
           y = "Number of stops")
  })
  
  output$plot_4 <- renderPlot({
    data_plot_4 <- data_reactive()$data_4
    
    ggplot(data_plot_4, aes(x = reorder(route_id, -dist_between_st), y = dist_between_st)) +
      geom_bar(stat = "identity", position = "dodge", fill = "yellow3") +
      labs(title = "TOP lines with the shortest average distance between stops",
           x = "Line number",
           y = "Average distance between stops")
  })
  
  output$map2 <- renderLeaflet({
    bins_przyjazdy <- seq(floor(min(top_stops$liczba_przyjazdow, na.rm = TRUE)), ceiling(max(top_stops$liczba_przyjazdow, na.rm = TRUE)), length.out = 10)
    bins_linie <- unique(quantile(top_stops$liczba_linii, probs = seq(0, 1, length.out = 5), na.rm = TRUE))
    
    leaflet(data = top_stops) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = 6,
        fillColor = ~colorBin(palette = "YlOrRd", domain = top_stops$liczba_przyjazdow, bins = bins_przyjazdy, na.color = "#FFFFFF")(liczba_przyjazdow),
        color = "transparent",
        fillOpacity = 0.8,
        popup = ~paste0("<b>Stop: </b>", stop_name, "<br><b>Number of arrivals: </b>", liczba_przyjazdow, "<br><b>Number of routes: </b>", liczba_linii)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = colorBin(palette = "YlOrRd", domain = top_stops$liczba_przyjazdow, bins = bins_przyjazdy, na.color = "#FFFFFF"),
        values = top_stops$liczba_przyjazdow,
        title = "Number of arrivals",
        opacity = 1.0
      )
  })
  
  observe({
    if (input$legend == "przyjazdy") {
      bins <- seq(floor(min(top_stops$liczba_przyjazdow, na.rm = TRUE)), ceiling(max(top_stops$liczba_przyjazdow, na.rm = TRUE)), length.out = 10)
      pal <- colorBin(palette = "YlOrRd", domain = top_stops$liczba_przyjazdow, bins = bins, na.color = "#FFFFFF")
      title <- "Number of arrivals"
      values <- top_stops$liczba_przyjazdow
      bins <- round(bins)
    } else {
      bins <- unique(quantile(top_stops$liczba_linii, probs = seq(0, 1, length.out = 5), na.rm = TRUE))
      pal <- colorBin(palette = "YlOrRd", domain = top_stops$liczba_linii, bins = bins, na.color = "#FFFFFF")
      title <- "Number of routes"
      values <- top_stops$liczba_linii
      bins <- round(bins)
    }
    
    leafletProxy("map2", data = top_stops) %>%
      clearMarkers() %>%
      addCircleMarkers(
        radius = 6,
        fillColor = ~pal(if (input$legend == "przyjazdy") liczba_przyjazdow else liczba_linii),
        color = "transparent",
        fillOpacity = 0.8,
        popup = ~paste0("<b>Stop: </b>", stop_name, "<br><b>Number of arrivals: </b>", liczba_przyjazdow, "<br><b>Number of routes: </b>", liczba_linii)
      ) %>%
      clearControls() %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = round(values),
        title = title,
        opacity = 1.0
      )
  })
  output$barPlot <- renderPlotly({
    p <- ggplot(miejscowosci_czas_oczekiwania, aes(x = reorder(miejscowosc, sredni_czas_oczekiwania), y = sredni_czas_oczekiwania, 
                                                   text = paste0(miejscowosc, "\nAvarage waiting time: ", round(sredni_czas_oczekiwania, 2), " min"))) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +
      labs(title = "Avarage waiting time on the stop in each suburban village",
           x = "Village",
           y = "Avarage time (min)") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>% 
      config(displayModeBar = FALSE) %>%
      layout(hovermode = "closest")
  })
}

uiKuba <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "transport",
        label = "Transport type:",
        choices = c(
          "bus" = 1 ,
          "tram" = 2,
          "SKM" = 3
        )
      ),
      selectInput(
        inputId = "days",
        label = "",
        choices = c(
          "weekdays",
          "weekends and holidays" = "weekendsAndHolidays"
        )
      ),
      width = 3
    ),
    mainPanel(leafletOutput("map1",height = "800px", width = "100%"))
  ))

uiAla <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("type_of_transport", label = "Choose type of transport:",
                         choices = list("tram" = 1, 
                                        "regular line" = 2, 
                                        "regular periodic line" = 3, 
                                        "accelerated periodic line" = 4,
                                        "accelerated line" = 5,
                                        "regular zone line" = 6,
                                        "periodic zone line" = 7,
                                        "special and additional line" = 8,
                                        "night line" = 9,
                                        "periodic express line" = 10,
                                        "local suburban line supplementary" = 11,
                                        "replacement line" = 12,
                                        "train" = 13,
                                        "subway" = 14),
                         selected = 2)
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("plot_1")),
        column(6, plotOutput("plot_2"))
      ),
      fluidRow(
        column(6, plotOutput("plot_3")),
        column(6, plotOutput("plot_4"))
      )
    )
  )
)

uiMateusz1 <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons("legend", "Choose:",
                   choices = list("Number of arrivals" = "przyjazdy",
                                  "Number of routes" = "linie"))
    ),
    mainPanel(
      leafletOutput("map2",height = "800px", width = "100%")
    )
  )
)

uiMateusz2 <- fluidPage(
  mainPanel(
    plotlyOutput("barPlot",height = "800px", width = "100%")
  )
)

ui <- navbarPage("Warsaw Public Transport",
                 tabPanel("Avarage time between arrivals on each stop/station",uiKuba),
                 tabPanel("1% of most frequently visited stops", uiMateusz1),
                 tabPanel("Transport availability in suburban villages around Warsaw", uiMateusz2),
                 tabPanel("TOP lines",uiAla)
                 )

runApp(shinyApp(ui, server))
