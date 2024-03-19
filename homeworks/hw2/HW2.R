spotify <- read.csv("spotify-2023.csv")
View(spotify)
#install.packages('lubridate')
library(dplyr)
library(ggplot2)
library(lubridate)



# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

zad_1 <- spotify%>%
  filter(released_month %in% c(1, 2, 3))%>%
  filter(released_year == 2023)%>%
  select(artist_count, streams)%>%
  mutate(artist_count = as.character(artist_count))%>%
  mutate(streams = as.numeric(streams)/ 100000) # skaluje przez 100 000 
  
  

ggplot(zad_1, aes(x = streams, color = artist_count)) +
  geom_density() +
  xlim(0, 3500)

ggplot(zad_1, aes(x = artist_count, y = streams)) +
  geom_boxplot()

# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

spotify%>%
  filter(released_year %in% c(2019,2020,2021,2022))%>%
  mutate(release_date = make_date(released_year, released_month, released_day),
         day_of_week = wday(release_date, label = TRUE))%>%
  group_by(released_year, day_of_week)%>%
  summarise(n_songs = n())%>%
  ggplot( aes(x = day_of_week, y = n_songs, fill = as.factor(released_year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Rozkład liczby wypuszczonych piosenek względem dnia tygodnia (2019-2022)",
       x = "Dzień tygodnia",
       y = "Liczba piosenek",
       fill = "Rok")



# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?



zad_3 <- spotify%>%
  mutate(odtwarzanie = as.numeric(streams) / in_spotify_playlists)

top_20_percent_threshold <- quantile(zad_3$odtwarzanie, 0.8, na.rm = TRUE)

zad_3%>%
  filter(odtwarzanie >= top_20_percent_threshold)


  ggplot(zad_3, aes(x = bpm, fill = as.factor(mode))) +
  geom_histogram(binwidth = 1, position = 'identity', alpha = 0.6) +
  labs(title = "Rozkład tempa (BPM) względem skali dla 20% najczęściej odtwarzanych piosenek",
       x = "Tempo (BPM)",
       y = "Liczba Piosenek",
       fill = "Skala") +
  theme_minimal()+
  facet_wrap(~ mode)


  
  ggplot(zad_3, aes(x = mode, y = bpm)) +
    geom_boxplot()
  
  
  ggplot(zad_3, aes(x = bpm , color =  mode)) +
    geom_density() +
    xlim(0, 250)



# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

zad_4 <- spotify%>%
    mutate(streams = as.numeric(streams))%>%
    group_by(artist.s._name)%>%
    summarise(suma_odtworzen = sum(streams, na.rm = TRUE))%>%
    arrange(desc(suma_odtworzen))%>%
    head(10)
  
ggplot(zad_4, aes(x = reorder(artist.s._name, suma_odtworzen), y = suma_odtworzen)) +
    geom_bar(stat = "identity") +
    labs(title = "Top 10 Artystów wg Łącznej Liczby Odtworzeń",
         x = "Artysta",
         y = "Łączna Liczba Odtworzeń") +
    theme_minimal() +
    coord_flip() 

# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify%>%
  mutate(artist_count = as.character(artist_count))%>%
  ggplot(aes(x = `danceability_.`, y = `energy_.`, color = artist_count)) + 
  geom_point() +
  labs(x = "Taneczność", 
       y = "Energetyczność", 
       color = "Ilosc artystow",
       title = "Zależność taneczności od energetyczności") +
  theme_bw() +
  theme(legend.position = "top") +
  facet_wrap(~ artist_count, ncol = 4)



# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

most_popular_songs_each_year <- spotify %>%
  filter(released_year >= 2013, released_year <= 2023) %>% # Filtrujemy lata 2013-2023
  group_by(released_year, track_name) %>%
  summarise(total_streams = sum(as.numeric(streams), na.rm = TRUE))%>%
  arrange(desc(released_year), desc(total_streams))%>%
  group_by(released_year)%>%
  slice(1)%>%
  View()



# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

zad_7_spotify <- spotify%>%
  filter(released_year == 2022)%>%
  group_by(released_month)%>%
  summarise(suma_spotify = sum(in_spotify_playlists))

zad_7_apple <- spotify%>%
  filter(released_year == 2022)%>%
  group_by(released_month)%>%
  summarise(suma_apple = sum(in_apple_playlists))

zad_7_1 <- merge(zad_7_apple, zad_7_spotify, by = "released_month")

#Zdaję sobie sprawę ze poniższy sposób jest mało optymalny jednak nie byłam w stanie wpaść
# na lepszy pomysł 
zad_7 <- data.frame(
  month = zad_7_1$released_month, 
  suma = c(zad_7_1$suma_spotify, zad_7_1$suma_apple),
  sp_or_ap = c(rep("spotify", 12), rep("apple", 12))
)


ggplot(zad_7,  aes(x = month, y = suma, fill = as.factor(sp_or_ap))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "suma piosenek na playlistach w danym miesiącu w roku 2022 ",
       x = "miesiac",
       y = "Liczba piosenek",
       fill = "spotify / apple")








