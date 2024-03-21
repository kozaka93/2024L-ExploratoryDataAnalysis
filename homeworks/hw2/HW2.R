library(dplyr)
library(ggplot2)

spotify <- read.csv("C:/Users/mateu/OneDrive/Dokumenty/R laby/spotify-2023.csv")
spotify$streams <- as.numeric(gsub("[^0-9]", "", spotify$streams))
# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?


spotify %>% 
  filter(released_year == 2023, released_month %in% c(1, 2, 3)) %>%
  group_by(artist_count) %>%
  summarise(TotalStreams = sum(streams)) %>%
  ggplot(aes(x = factor(artist_count), y = TotalStreams)) +
  geom_col() +
  labs(title = "Rozkład liczby odtworzeń w Q1 2023 w zależności od liczby wykonawców",
       x = "Liczba wykonawców",
       y = "Łączna liczba odtworzeń")

#Wnioski: Można zauważyć że liczba odtworzeń jest największa dla piosenek z dwoma wykonawcami , nie nacznie mniejszy jest dla jednego wykonawcy.






# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?


spotify %>%

  mutate(released_date = as.Date(paste(released_year, released_month, released_day, sep = "-")),

         weekday = weekdays(released_date)) %>%

  filter(released_year >= 2019 & released_year <= 2022) %>%

  count(released_year, weekday) %>%
  ggplot(aes(x = weekday, y = n, fill = as.factor(released_year))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Rozkład liczby wydanych piosenek względem dnia tygodnia (2019-2022)",
       x = "Dzień tygodnia", y = "Liczba piosenek",fill = "Rok wydania") 

#Wnioski:Zdecydowanie najwięcej jest wydawanych piosenek w piątek, wśród innych dni tygodnia nieznacznie wybija się jeszcze czwartek,można podejrzewać że te piosenki są najczęściej wydawana centralnie przed weekendem.




# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?



top_20_percent_threshold <- quantile(spotify$in_spotify_playlists, 0.8)

top_20_percent_songs <- spotify %>%
  filter(in_spotify_playlists >= top_20_percent_threshold)


ggplot(top_20_percent_songs, aes(x = mode, y = bpm, fill = mode)) +
  geom_boxplot() +
  labs(title = "Rozkład BPM względem skali dla top 20% piosenek na playlistach Spotify",
       x = "Skala", y = "Tempo (BPM)")

#Wnioski:Wykres przedstawia rozkład tempa (BPM) utworów w zależności od skali muzycznej (major lub minor) dla 20% najczęściej odtwarzanych piosenek na Spotify. Można zauważyć, że mediany BPM dla obu skal są podobne, jednak rozkład dla skali minorowej wydaje się być nieco szerszy, co sugeruje większe zróżnicowanie tempa w tej skali. Wyższe wartości skrajne w skali minorowej mogą wskazywać na obecność utworów z szybszym tempem, co może oddziaływać na emocjonalny charakter muzyki. Ogólnie, utwory w skali durowej (major) wydają się mieć bardziej skoncentrowane tempo, z mniejszym zakresem wariacji. 


# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?


spotify %>%
  group_by(artist.s._name) %>%
  summarise(total_streams = sum(streams)) %>%
  arrange(desc(total_streams)) %>%
  slice(1:10) %>% 
  ggplot(aes(x = reorder(artist.s._name, total_streams), y = total_streams)) +
  geom_col() +
  coord_flip() +
  scale_y_log10()+
  labs(title = "Top 10 artystów z największą liczbą odtworzeń w skali logarytmicznej",
       x = "Artysta", y = "Łączna liczba odtworzeń") 
 
#Wnioski:Najwięcej odtworzeń w liczbach bezwzględnych,zdecydowanie Edison Lighthouse, inni wykonawcy mają stosunkowe zbliżone wyniki co szczególnie dobrze pokazuje skala logarytmiczna.




# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

ggplot(spotify, aes(x = danceability_., y = energy_., color = as.factor(artist_count))) +
  geom_point(alpha = 0.6) + # Używamy przezroczystości, aby lepiej zobaczyć nakładające się punkty
  labs(title = "Zależność energetyczności od taneczności w zależności od liczby artystów",
       x = "Taneczność (%)",
       y = "Energetyczność (%)", color="Liczba artystów")



#pomocniczy
ggplot(spotify, aes(x = danceability_., y = energy_., color = as.numeric(artist_count))) +
  geom_point(alpha = 0.6) +
  scale_color_gradient(low = "lightblue", high = "red", name = "Liczba artystów") +
  labs(title = "Zależność energetyczności od taneczności w zależności od liczby artystów",
       x = "Taneczność (%)",
       y = "Energetyczność (%)")


#Wnioski:można zauważyć że wraz liczbą artystów zasaniczo rośnie i taneczność i energiczność.



# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?



spotify %>%
  filter(released_year%in% 2013:2023) %>% 
  

spotify %>%
  filter(released_year %in% 2013:2023) %>%
  group_by(released_year) %>%
  filter(streams == max(streams)) %>%
  ungroup() %>%
  mutate(track_name = factor(track_name, levels = track_name[order(released_year)])) %>%
  select(released_year, track_name, artist.s._name, streams) %>%
  ggplot(aes(x = track_name, y = streams, fill = as.factor(released_year))) +
  geom_col() +
  labs(title = "Najbardziej popularna piosenka w każdym roku (2013-2023)",
       x = "Tytuł",
       y = "Liczba odtworzeń",
       fill = "Rok wydania") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


#Wnioski:Można zauważyć wieksze wartości najpopularniejszych tytułów w 2016- 2019 roku może być to spowodowane większą liczbą użytkowników, warte nadmienienia jest też że zaskakujaco mało ma utwór z 2023 roku ale prawdopodbnie jest to związane z tym że nie mam danych do końca roku tutaj.




# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?



spotify_2022 <- spotify %>%
  filter(released_year == 2022)

# Sumujemy liczby piosenek na playlistach Spotify i Apple Music dla każdego miesiąca
monthly_summary <- spotify_2022 %>%
  group_by(released_month) %>%
  summarise(Spotify_Playlist_Sum = sum(in_spotify_playlists),
            Apple_Playlist_Sum = sum(in_apple_playlists))


ggplot(monthly_summary, aes(x = as.factor(released_month))) +
  geom_col(aes(y = Spotify_Playlist_Sum , colour = "Spotify Playlists")) +
  geom_col(aes(y = Apple_Playlist_Sum, colour = "Apple Playlists")) +
  labs(title = "Porównanie sumy piosenek na playlistach Spotify i Apple Music w 2022 roku",
       x = "Miesiąc", y = "Suma piosenek na playlistach") +
  scale_fill_manual("", 
                    values = c("Spotify Playlists" = "blue", "Apple Playlists" = "red"))




#Wnioski:Widzimy że zdecydowanie spotify jest popularniejszą platformą.