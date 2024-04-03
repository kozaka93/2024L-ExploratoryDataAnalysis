spotify <- read.csv("spotify-2023.csv")

library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(forcats)
library(tidyr)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>% 
  filter(released_year == 2023, released_month %in% c(1,2,3)) %>% 
  mutate(streams = as.numeric(streams)) %>% 
  ggplot(aes(factor(artist_count), streams)) +
  geom_violin(fill = "#ff5cec", color = "#b325b6") +
  geom_boxplot(width = 0.1, fill = "#00da62") +
  scale_y_continuous(labels = label_number()) +
  labs(title = "Rozkład liczby odtworzeń piosenek opublikowanych w pierwszym kwartale 2023 roku",
       subtitle = "w zależności od liczby wykonawców",
       x = "Liczba wykonawców",
       y = "Liczba odtworzeń piosenek")

# Dla każdej liczby wykonawców mediana liczby odtworzeń jest podobna. Rozstęp miedzykwartylowy jest największy w przypadku piosenek 
# mających 2 wykonawców, a najmniejszy dla piosenek mających 3 wykonawców.
# Dla piosenek mających 1 wykonawcę można zaś zauważyc odstającą obserwację, która ma o wiele większą liczbę odtworzeń.



# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

spotify %>% 
  filter(released_year %in% 2019:2022) %>% 
  mutate(weekday = wday(paste(released_year, released_month, released_day, sep = "-"), label=TRUE, abbr=FALSE, week_start=1)) %>%
  ggplot(aes(factor(weekday))) +
  geom_bar() +
  facet_wrap(~released_year, scales = "free_y") +
  labs(title = "Rozkład liczby wypuszczonych piosenek względem dnia tygodnia",
       subtitle = "w latach 2019-2022",
       x = "dzień tygodnia",
       y = "Liczba wypuszczonych piosenek")

# W każdym roku piosenek wypuszczonych w piątek jest zdecydowanie więcej niż w pozostałe dni tygodnia.
# Drugim dniem tygodnia z największą liczbą wypuszczonych piosenek był czwartek. Liczba piosenek 
# wypuszczanych w pozostałe dni tygodnia zależy od roku.



# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?


spotify %>% 
  mutate(spp = as.numeric(streams)/in_spotify_playlists) %>% 
  filter(spp >= quantile(spp, 0.8, na.rm = TRUE)) %>% 
  ggplot(aes(mode, bpm)) +
  geom_violin(fill = "lightpink", color = "hotpink") +
  geom_boxplot(width = 0.2, fill = "#00da62") +
  labs(title = "Rozkład tempa względem skali dla piosenek",
       subtitle = "będących wśród 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist",
       x = "Skala",
       y = "Tempo")

# Dla obu skali mediana wynosi około 125 bpm. Rozstęp miedzykwartylowy jest większy dla skali Major,
# jednakże dla skali Minor występują obserwacje odstające, które mają większe tempo.



# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

spotify %>% 
  mutate(streams = as.numeric(streams)) %>% 
  group_by(artist.s._name) %>% 
  summarise(total_streams = sum(streams)) %>% 
  arrange(-total_streams) %>% 
  head(10) %>% 
  ggplot(aes(fct_reorder(artist.s._name, -total_streams), total_streams)) +
  geom_col(fill = "#265485") +
  scale_y_continuous(labels = label_number()) +
  labs(title = "Top 10 artystów mających najwięcej odtworzeń wszystkich swoich piosenek",
       x = "Artysta",
       y = "Łączna liczba odtworzeń")

# artist.s._name  total_streams
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




# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>% 
  ggplot(aes(x=danceability_., y=energy_.)) +
  geom_point() +
  facet_wrap(~artist_count) +
  lims(x=c(0, 100), y=c(0, 100)) +
  labs(title = "Zależność energetyczności od taneczności w zależności od liczby artystów",
       x = "Taneczność",
       y = "Energetyczność")

# Zazwyczaj im piosenka jest bardziej taneczna, tym bardziej energetyczna.


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

spotify %>% 
  filter(released_year %in% 2013:2023) %>% 
  mutate(streams = as.numeric(streams)) %>% 
  group_by(released_year) %>%
  top_n(1, streams) %>% 
  select(track_name, streams) %>%
  mutate(top_songs = paste(track_name,released_year, sep = " - "),
         top_songs = fct_reorder(top_songs, released_year)) %>% 
  ggplot(aes(streams, top_songs)) +
  geom_col(fill = "#265485") +
  scale_x_continuous(labels = label_number()) +
  labs(title = "Najbardziej popularne piosenki w latach 2013 - 2023",
       x = "Liczba odtworzeń",
       y = "Tytuł piosenki - Rok")


#   released_year track_name                   streams
# 1          2023 Flowers                   1316855716
# 2          2022 As It Was                 2513188493
# 3          2019 Blinding Lights           3703895074
# 4          2020 Heat Waves                2557975762
# 5          2021 STAY (with Justin Bieber) 2665343922
# 6          2018 Someone You Loved         2887241814
# 7          2016 One Dance                 2713922350
# 8          2017 Shape of You              3562543890
# 9          2013 Take Me To Church         2135158446
# 10          2014 Thinking Out Loud         2280566092
# 11          2015 Love Yourself             2123309722



# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

spotify %>% 
  filter(released_year == 2022) %>% 
  group_by(released_month) %>% 
  summarise(spotify_sum = sum(in_spotify_playlists),
            apple_sum = sum(in_apple_playlists)) %>% 
  pivot_longer(!released_month, names_to="platform", values_to="sum") %>% 
  ggplot(aes(factor(released_month), sum, fill = platform)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Suma piosenek na playlistach na spotify i apple w 2022 roku",
       x = "Miesiąc",
       y = "Suma piosenek na playlistach") +
  scale_fill_discrete(name = "Platforma", labels = c("apple", "spotify"))

# W każdym miesiącu suma piosenek na playlistach na spotify była zdecydowanie większa niż na apple.
# Można również zaobserwować, że suma piosenek na playlistach była największa w maju na apple i na spotify.

