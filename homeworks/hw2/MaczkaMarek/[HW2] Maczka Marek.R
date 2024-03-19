spotify <- read.csv("spotify-2023.csv")

library(dplyr)
library(ggplot2)
# install.packages("lubridate")
library(lubridate)
library(scales)
library(tidyr)
library(ggthemes)
library(forcats)


# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>% 
  filter(released_year == 2023, released_month %in% 1:3) %>%
  mutate(streams = as.integer(streams)) %>% 
  ggplot(aes(x=factor(artist_count), y=streams)) +
  geom_violin(fill="pink", color="hotpink")+
  geom_boxplot(width=0.1, fill="lightgreen", color="springgreen4")+
  scale_y_continuous(labels = label_number())+
  labs(title = "Rozkład liczby odtworzeń piosenek opublikowanych w roku 2023",
       subtitle = "w pierwszym kwartale w zależności od liczby wykonawców",
       x = "Liczba wykonawców",
       y = "Liczba odtworzeń piosenek")

# Z wykresu możemy zauważyć, że niezależnie od liczby wykonawców mediana liczby 
# odtworzeń piosenek jest mniej więcej na tym samym poziomie. Odchylenie ćwiartkowe
# liczby odtworzeń jest największe dla liczby wykonawców równej 2; najmniejsze
# zaś dla liczby wykonawców równej 3. Możemy zauważyć, że najmniejszy rozrzut wyników
# ma opcja z trzema wykonawcami, największy z wartościami (bardzo) odstającymi
# opcja z jednym wykonawcą.


# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

spotify %>% 
  filter(released_year >= 2019, released_year <= 2022) %>% 
  mutate(full_date = paste(released_year, released_month, released_day, sep = "-"),
         day_of_week = wday(full_date, label=TRUE, abbr=FALSE, week_start=1)) %>% 
  ggplot(aes(x=day_of_week)) + 
  geom_bar(fill="turquoise") +
  coord_flip()+ 
  scale_x_discrete(limits = rev)+
  labs(title = "Rozkład liczby wypuszczonych piosenek względem dnia tygodnia",
       subtitle = "dla poszczególnych lat między 2019 a 2022",
       x = "Dzień tygodnia",
       y = "Liczba wypuszczonych piosenek") +
  facet_wrap(~released_year, nrow = 2, scales = "free_x")

# Niezależnie od roku, najwięcej piosenek było wypuszczane w piątki (z dużą różnicą od pozostałych dni), 
# drugim najbardziej popularnym dniem był czwartek - również niezależnie do roku.
# Rozkład w pozostałych dniach różni się w zależności od roku.


# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

spotify %>% 
  filter(streams != "BPM110KeyAModeMajorDanceability53Valence75Energy69Acousticness7Instrumentalness0Liveness17Speechiness3") %>%
  mutate(rat=as.numeric(streams)/in_spotify_playlists) %>% 
  filter(rat > quantile(rat, 0.8)) %>% 
  ggplot(aes(x=mode, y=bpm))+
  geom_violin(fill="skyblue1", color="skyblue3")+
  geom_boxplot(width=0.1, fill="palegreen", color="springgreen4")+
  labs(title = "Rozkład tempa względem skali dla piosenek,",
       subtitle = "które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify",
       x = "Skala",
       y = "Tempo (bpm)")

# Mediana i moda tempa (wyrażowanego w bpm) przyjmuje podobne wartości, 
# (niezależnie od skali), bliskie 125-130 bpm. Rozkłady są całkiem zbliżone.
# Dla skali Major nie ma wartości odstający, dla skali Minor są dwie.

# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

spotify %>% 
  filter(streams != "BPM110KeyAModeMajorDanceability53Valence75Energy69Acousticness7Instrumentalness0Liveness17Speechiness3") %>%
  mutate(streams = as.numeric(streams)) %>% 
  group_by(artist.s._name) %>% 
  summarise(total_streams = sum(streams)) %>% 
  arrange(-total_streams) %>%
  head(10) %>% 
  mutate(artist.s._name = fct_reorder(artist.s._name, total_streams)) %>% 
  ggplot(aes(x = total_streams, y = artist.s._name)) +
  geom_col(fill = "springgreen3") +
  scale_x_continuous(labels = label_number()) +
  labs(title = "Top 10 artystów, którzy mają najwięcej odtworzeń wszystkich swoich piosenek",
       x = "Łączna liczba odtworzeń",
       y = "Artysta")

# Odp.
# artist.s._name  total_streams
# 
# 1 The Weeknd        14185552870
# 2 Taylor Swift      14053658300
# 3 Ed Sheeran        13908947204
# 4 Harry Styles      11608645649
# 5 Bad Bunny          9997799607
# 6 Olivia Rodrigo     7442148916
# 7 Eminem             6183805596
# 8 Bruno Mars         5846920599
# 9 Arctic Monkeys     5569806731
# 10 Imagine Dragons    5272484650

# Najwięcej odtworzeń mają kolejno:
# The Weeknd, Taylor Swift, Ed Sheeran, Harry Styles, Bad Bunny, Olivia Rodrigo,
# Eminem, Bruno Mars, Arctic Monkeys, Imagine Dragons.


# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>% 
  ggplot(aes(x=danceability_., y=energy_.)) +
  geom_point()+
  lims(x=c(0, 100), y=c(0, 100))+
  facet_wrap(~artist_count) +
  labs(title = "Zależność energetyczności od taneczności w zależności od liczby artystów",
       x = "Taneczność",
       y = "Energetyczność")

  
# Próbowałem też nie rozbijać na kilka wykresów, choć wówczas wykres staje się mało czytelny
# 
# spotify %>% 
#   ggplot(aes(x=danceability_., y=energy_., color=factor(artist_count))) +
#   geom_point()+
#   lims(x=c(0, 100), y=c(0, 100)) +
#   labs(title = "Zależnośc energetyczności od taneczności w zależności od liczby artystów",
#      x = "Taneczność",
#      y = "Energetyczność",
#      color = "Liczba artystów")

# Moim zdaniem, ciężko jest jakoś jednoznacznie określić jak zależy 
# energetyczność od tanecznośći patrząc przez pryzmat liczby artystów.
# Można spróbować wysnuć wniosek, że energetyczność co da zasady rośnie wraz ze
# wzrostem taneczności - choć niespecjalnie to widać po danych.


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

# spotify %>%
#   filter(released_year >= 2013, released_year<=2023) %>%
#   mutate(streams=as.numeric(streams)) %>%
#   group_by(released_year) %>%
#   filter(streams == max(streams)) %>% 
#   arrange(released_year) %>%
#   select(track_name, released_year, streams)

spotify %>% 
  filter(released_year >= 2013, released_year<=2023) %>% 
  mutate(streams=as.numeric(streams)) %>% 
  group_by(released_year) %>% 
  top_n(1, streams) %>% 
  arrange(released_year) %>% 
  select(track_name, released_year, streams) %>% 
  mutate(released_year = factor(released_year)) %>% 
  ggplot(aes(x = streams, y = released_year)) +
  geom_col(fill = "skyblue3") +
  scale_x_continuous(labels = label_number()) +
  scale_y_discrete(limits = rev) +
  geom_text(aes(x = streams*0.8, label = track_name)) +
  labs(title = "Najpopularniejsze piosenki w latach 2013 - 2023",
       x = "Liczba odtworzeń",
       y = "Rok")

#    track_name                released_year    streams

# 1  Take Me To Church                  2013 2135158446
# 2  Thinking Out Loud                  2014 2280566092
# 3  Love Yourself                      2015 2123309722
# 4  One Dance                          2016 2713922350
# 5  Shape of You                       2017 3562543890
# 6  Someone You Loved                  2018 2887241814
# 7  Blinding Lights                    2019 3703895074
# 8  Heat Waves                         2020 2557975762
# 9  STAY (with Justin Bieber)          2021 2665343922
# 10 As It Was                          2022 2513188493
# 11 Flowers                            2023 1316855716

# Najpopularniejszą piosenką była:
# Take Me To Church w 2013, Thinking Out Loud w 2014, Love Yourself w 2015,
# One Dance w 2016, Shape of You w 2017, Someone You Loved w 2018, Blinding Lights w 2019,
# Heat Waves w 2020, STAY (with Justin Bieber) w 2021, As It Was w 2022, Flowers w 2023


# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

# Bez skali logarytmicznej

spotify %>% 
  filter(released_year == 2022) %>% 
  group_by(released_month) %>% 
  summarise(Spotify = sum(in_spotify_playlists),
            Apple = sum(in_apple_playlists)) %>%
  pivot_longer(-released_month, names_to="variable", values_to="value") %>% 
  ggplot(aes(x=factor(released_month), y=value, fill=variable))+
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Porównanie sum piosenek na playlistach na spotify i apple w 2022",
       x = "Miesiąc",
       y = "Suma piosenek na playlistach",
       fill = "Platforma")


# Ze skalą logarytmiczną

spotify %>% 
  filter(released_year == 2022) %>% 
  group_by(released_month) %>% 
  summarise(Spotify = sum(in_spotify_playlists),
            Apple = sum(in_apple_playlists)) %>%
  pivot_longer(-released_month, names_to="variable", values_to="value") %>% 
  ggplot(aes(x=factor(released_month), y=value, fill=variable))+
  geom_bar(position="dodge", stat="identity") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  labs(title = "Porównanie sum piosenek na playlistach na Spotify i Apple w 2022",
       x = "Miesiąc",
       y = "Suma piosenek na playlistach",
       fill = "Platforma")

# Z pierwszego wykresu widać, że w każdym miesiącu liczba piosenek na playlistach 
# Spotify jest znacznie większa niż na playlistach Apple'a.
# Z drugiego wykresu możemy zaś zauważyć, że największa suma piosenek na playlistach
# była w maju a najmniejsza we wrześniu, zarówno dla Spotify jak i Apple'a.

# Miłego dzionka oraz smacznej kawusi/ciepłej herbatki dla sprawdzającego/sprawdzającej :)
