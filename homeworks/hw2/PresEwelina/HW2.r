
spotify <- read.csv("spotify-2023.csv")

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(forcats)


# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>% 
  filter(released_year == 2023, released_month <= 3) %>% 
  ggplot(aes(x = as.character(artist_count), y = as.numeric(streams))) +
  geom_boxplot(fill = "darkgreen") +
  labs(title = "Rozkład liczby odtworzeń piosenek w zależności od liczby wykonawców",
       subtitle = " w I kwartale 2023 roku",
       x = "Liczba wykonawców",
       y = "Liczba odtworzeń") +
  theme_bw()

#Można zauważyć, że piosenki komponowane przez 2 artystów mają nieco większą madianę 
#oraz znacząco większy 3 kwartyl liczby odtworzeń.
#Mamy też jedną mocno odstającą wartość - pewna piosenka, która ma jednego autora,
#miała znacznie większą liczbę odwtorzeń niż inne piosenki opublikowane w 2023 roku.


# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

spotify %>% 
  filter(released_year >= 2019, released_year <= 2022) %>% 
  mutate(date = as.POSIXlt(paste(as.character(released_year), 
                                 as.character(released_month), 
                                 as.character(released_day), sep = "-"))) %>% 
  mutate(week_day = wday(date, week_start = 1)) %>% 
  group_by(released_year, week_day) %>% 
  summarise(song_count = n(), .groups = "drop") %>% 
  ggplot(mapping = aes(x = factor(week_day, labels = c("Pon", "Wt", "Śr", "Czw", "Pt", "Sob", "Niedz")), y = song_count)) +
  geom_col(fill = "navyblue") +
  labs(title = "Rozkład liczby wypuszczonych piosenek względem dnia tygodnia",
       subtitle = "dla lat 2019-2022",
       x = "Dzień tygodnia",
       y = "Liczba wypuszczonych piosenek") +
  facet_wrap(~as.character(released_year), scales = "free_y")
  
#W każdym z lat 2019-2022 najwięcej piosenek (spośród tych występujących w naszym zbiorze danych) 
#było wypuszczanych w piątek.
#Dla lat 2019 i 2020 mamy znacząco mniej danych niż dla 2021 i 2022 (np. dla 2019 roku zaledwie 36 piosenek).
#Nie powinniśmy więc wysnuwać zbyt daleko idących wniosków analizując te 2 wykresy.


# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

spotify %>% 
  mutate(streams_playlist = as.numeric(streams)/in_spotify_playlists) %>% 
  slice_max(order_by = streams_playlist, n = round(nrow(spotify) * 0.2)) %>% 
  ggplot(aes(x = as.factor(mode), y = bpm)) +
  geom_boxplot(fill = "wheat") +
  labs(title = "Rozkład tempa względem skali",
       subtitle = "dla piosenek, które są w 20% najczęściej odtwarzanych utworów w przeliczeniu na liczbę playlist spotify",
       x = "Skala",
       y = "Tempo (bpm)") +
  theme_bw()

#Dla obu skal mediana ma wartość około 125 bpm.
#Dla skali durowej mamy nieco większy rozstęp międzykwartylowy.


# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

spotify %>% 
  group_by(artist.s._name) %>% 
  summarise(streams_sum = sum(as.numeric(streams))) %>% 
  arrange(-streams_sum) %>% 
  head(10) %>% 
  ggplot(mapping = aes(x = fct_reorder(artist.s._name, streams_sum, .desc = TRUE), y = streams_sum)) +
  geom_col(fill = "skyblue4") +
  labs(title = "Top 10 artystów z największą liczbą odtworzeń utworów",
       x = "Artysta",
       y = "Suma odtworzeń utworów") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw()

#Na wykresie od lewej do prawej artyści z największą liczbą odwtorzeń.
#Dobrym sposobem wizualizacji byłaby też tabelka.

# artist.s._name  streams_sum
#
# 1 The Weeknd       14185552870
# 2 Taylor Swift     14053658300
# 3 Ed Sheeran       13908947204
# 4 Harry Styles     11608645649
# 5 Bad Bunny         9997799607
# 6 Olivia Rodrigo    7442148916
# 7 Eminem            6183805596
# 8 Bruno Mars        5846920599
# 9 Arctic Monkeys    5569806731
# 10 Imagine Dragons  5272484650


# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>% 
  ggplot(mapping = aes(x = danceability_., y = energy_., color = as.character(artist_count))) +
  geom_point() +
  labs(title = "Zależność energii i taneczności piosenek",
       subtitle = "w zależności od liczby autorów",
       x = "Taneczność",
       y = "Energia")+
  scale_color_manual(name = "Liczba artystów", values = c("1" = "orchid", "2" = "brown", "3" = "orange", "4" = "purple", 
                                                          "5" = "red", "6" =  "blue","7" = "green","8" = "darkred"))

#Wśród wielu piosenek duża taneczność idzie w parze z dużą energią, a mała taneczność z małą energią,
#ale jest też sporo piosenek, które nie spełniają tej reguły: 
#mają małą taneczność i dużą energię lub dużą taneczność i małą energię.
#Liczba artystów nie ma wpływu na zależność taneczności i energii.


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

spotify %>% 
  filter(released_year >= 2013, released_year <= 2023) %>% 
  group_by(released_year, track_name) %>% 
  summarise(streams_ = max(as.numeric(streams)), .groups = 'drop') %>%  #coś zamiast max?
  arrange(-streams_) %>% 
  group_by(released_year) %>% 
  slice_max(order_by = streams_, n = 1)

#W tym przypadku najlepszym sposobem wizualizacji jest tabelka.

# released_year track_name                  streams_
#
# 1          2013 Take Me To Church         2135158446
# 2          2014 Thinking Out Loud         2280566092
# 3          2015 Love Yourself             2123309722
# 4          2016 One Dance                 2713922350
# 5          2017 Shape of You              3562543890
# 6          2018 Someone You Loved         2887241814
# 7          2019 Blinding Lights           3703895074
# 8          2020 Heat Waves                2557975762
# 9          2021 STAY (with Justin Bieber) 2665343922
# 10         2022 As It Was                 2513188493
# 11         2023 Flowers                   1316855716


# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

spotify %>% 
  filter(released_year == 2022) %>% 
  group_by(released_month) %>% 
  summarise(spotify_playlist= sum(in_spotify_playlists), apple_playlist= sum(in_apple_playlists)) %>% 
  mutate(released_month = factor(month.name, levels = month.name)) %>% 
  pivot_longer(cols = c(spotify_playlist, apple_playlist)) %>% 
  ggplot(aes(y = released_month, x = value, fill = name)) +
  geom_col() +
  labs(title = "Suma piosenek na playlistach na Spotify oraz Apple Music",
       subtitle = "według miesięcy w 2022 roku",
       x = "Liczba piosenek",
       y = "Miesiąc") +
  scale_fill_discrete(name = "Platforma muzyczna", labels = c("Apple Music", "Spotify")) +
  theme_bw() 

#W każdym z miesięcy znacznie więcej piosenek było w sumie na playlistach Spotify.  
