library(dplyr)
library(ggplot2)
library(tidyr)

spotify <- read.csv("spotify-2023.csv")

options(scipen = 12)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>% 
  filter(released_year == 2023) %>% 
  filter(released_month %in% c(1, 2, 3)) %>% 
  ggplot(aes(x = as.character(artist_count), y = as.integer(streams))) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Rozkład liczby odtworzeń piosenek w zależności od liczby wykonawców",
       subtitle = "opublikowanych w roku 2023 w pierwszym kwartale",
       x = "Liczba wykonawców",
       y = "Liczba odtworzeń")

# Mediana odtworzeń dla każdej liczby wykonawców jest podobna.
# W przypadku pojedynczych wykonawców mamy te najbardziej odstające od mediany
# piosenki - te najpopularniejsze
# W przypadku dwóch wykonawców rozstęp międzykwartylowy jest największy, a trzech najmniejszy


# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

spotify %>% 
  filter(released_year %in% c(2019, 2020, 2021, 2022)) %>% 
  mutate(Date = as.Date(paste(released_year, released_month, released_day, sep = "-"))) %>% 
  mutate(weekday = factor(weekdays(Date), 
          levels = c("poniedziałek","wtorek","środa","czwartek","piątek","sobota","niedziela"))) %>% 
  group_by(released_year, weekday) %>% 
  summarise(song_count = n()) %>% 
  ggplot(aes(x = weekday, y = song_count)) +
  geom_col(fill = "chartreuse4") +
  facet_wrap(~as.character(released_year), scales = "free_y") +
  labs(title = "Rozkład wypuszczonych piosenek względem dnia tygodnia",
       subtitle = "w poszczególnych latach",
       x = "Dzień tygodnia",
       y = "Liczba wypuszczonych utworów")

# We wszystkich latach najwięcej piosenek wydano w piątek, 
# na drugim miejscu jest natomiast czwartek, 
# choć tych pionek wypuszcano już znacznie mniej
# W 2022 roku wydano znacznie więcej piosenek niż w latach 2019 - 2021


# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

spotify %>% 
  mutate(playlist_popularity = as.integer(streams) / in_spotify_playlists) %>%
  filter(playlist_popularity >= quantile(playlist_popularity, 0.8, na.rm = TRUE)) %>% 
  ggplot(aes(x = mode, y = bpm)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Rozkład tempa względem skali",
       subtitle = "dla piosenek, które są w 20% najczęściej odtwarzanych w przeliczeniu na liczbę playlist Spotify",
       x = "Skala",
       y = "Tempo (bpm)")

# Mediany w obu skalach są podobne - ok. 125 bpm
# Rozstęp międzykwartylowy jest większy dla piosenek durowych (major)
# niż dla molowych (minor), ale dla molowych występują wartości
# bardziej odstające - o wyższym tempie


# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

spotify %>% 
  group_by(artist.s._name) %>% 
  summarise(total_streams = sum(as.integer(streams), na.rm = TRUE)) %>% 
  arrange(- total_streams) %>% 
  head(10) %>% 
  ggplot(aes(x = total_streams, y = reorder(artist.s._name, total_streams))) +
  geom_col(fill = "chartreuse4") +
  labs(title = "Top 10 artystów z największą liczbą odtworzeń wszystkich piosenek",
       x = "Liczba odtworzeń",
       y = "Artysta")

# Najwięcej odtworzeń wszystkich swoich piosenek ma zdecydowanie Taylor Swift
# Na 2. miejscu jest The Weeknd
# A na 3. Bad Bunny
# 4. Olivia Rodrigo
# 5. Harry Styles
# 6. Eminem
# 7. Bruno Mars
# 8. Arctic Monkeys
# 9. Doja Cat
# 10. SZA

# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>% 
  ggplot(aes(x = energy_., y = danceability_., color = as.character(artist_count))) +
  geom_point() +
  scale_color_manual(values = c("deeppink", "darkmagenta", "deepskyblue", "black",
                                "chartreuse", "red", "yellow", "orange")) + 
  labs(title = "Energetyczność a taneczność przez pryzmat liczby artystów",
       x = "Energetyczność",
       y = "Taneczność",
       color = "Liczba artystów")

# Dla większej liczby artystów (5, 6, 7, 8) ciężko się wypowiedzieć, bo mamy mało danych
# (raptem po kilka piosenek)
# Dla 4 artystów też problematycznym jest wyciągnięcie wniosków,
# bo kilkanaście piosenek to dalej mało danych
# Dla 1, 2 lub 3 artystów też ciężko, bo widoczne na wykresie rozproszenie jest duże
# Natomiast można ogólnie stwierdzić, że
# im piosenka bardziej energetyczna, tym bardziej taneczna


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

spotify %>% 
  filter(released_year >= 2013 & released_year <= 2023) %>% 
  group_by(released_year) %>% 
  top_n(1, as.integer(streams)) %>% 
  ggplot(aes(x = as.integer(streams), y = reorder(track_name, - released_year), fill = as.character(released_year))) +
  geom_col() +
  scale_color_discrete(name = "Rok wydania") +
  labs(title = "Najpopularniejsze piosenki w latach 2013 - 2023",
       x = "Liczba odtworzeń",
       y = "Tytuł piosenki",
       fill = "Rok wydania")

# Najpopularniejsze piosenki w latach 2013 - 2023 to:
# 2013 - Take Me To Church
# 2014 - No Role Modelz
# 2015 - Love Yourself
# 2016 - There's Nothing Holdin' Me Back
# 2017 - HUMBLE
# 2018 - Call Out My Name
# 2019 - Circles
# 2020 - Levitating (feat. DaBaby)
# 2021 - good 4 u
# 2022 - Me Porto Bonito
# 2023 - Flowers

# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

spotify %>% 
  filter(released_year == 2022) %>% 
  group_by(released_month) %>% 
  summarise(sum_apple = sum(in_apple_playlists, na.rm = TRUE),
            sum_spotify = sum(in_spotify_playlists, na.rm = TRUE)) %>% 
  pivot_longer(cols = c(sum_apple, sum_spotify)) %>% 
  ggplot(aes(x = as.factor(released_month), y = value, group = name, fill = name)) +
  geom_col(position = "dodge") +
  labs(title = "Suma piosenek na playlistach Spotify a Apple w 2022",
       subtitle = "w poszczególnych miesiącach",
       x = "Numer miesiąca",
       y = "Liczba piosenek") +
  scale_fill_discrete(name="Platforma muzyczna", labels = c("Apple", "Spotify"))
  
# Zdecydowanie więcej piosenek jest na playlistach Spotify niż na playlistach Apple
# Najwięcej piosenek w roku 2022 wyszło w maju

