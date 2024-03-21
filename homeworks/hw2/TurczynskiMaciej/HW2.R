spotify <- read.csv('spotify-2023.csv')
head(spotify)
library(dplyr)
library(ggplot2)
# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?
data1 <- spotify %>% 
  filter(released_year == 2023 & released_month %in% c(1,2,3))

ggplot(data1, aes(x = as.character(artist_count), y = as.integer(streams)/1000000)) +
  geom_boxplot(fill = 'green') +
  labs(title = 'Rozkład liczby odtworzeń piosenek w zależności od liczby wykonawców',
       x = 'Liczba wykonawców',
       y = 'Liczba odtworzeń na spotify w milionach')
  


#najwięcej odtworzeń mają piosenki z 2 wykonawcami, zaś piosenki pisane przez 1 wykonawcę mają mocne outlinery,
#są to piosenki bardzo popularne


# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?
library(lubridate)
data2 <- spotify %>% 
  filter(released_year %in% c(2019,2020,2021,2022)) %>% 
  mutate(released_day_of_week = factor(weekdays(ymd(paste(released_year, released_month, released_day, sep = '-'))),
                                       levels = weekdays(seq(as.Date('1900-01-01'),as.Date('1900-01-07'), by = 1)))) %>% 
  group_by(released_year, released_day_of_week) %>% 
  summarise(n = n())

ggplot(data2, aes(x = released_day_of_week, y = n)) +
  geom_col(fill = 'red') +
  labs(title = 'Rozkład liczby wypuszczonych piosenek względem danego dnia tygodnia',
       x = 'Dzień tygodnia',
       y = 'Liczba wypuszczonych utworów') +
  facet_wrap(~as.character(released_year), scales = 'free_y')

#najwięcej piosenek wydano w każdym roku w piątek, w resztę dni mniej oraz zdarzyło się, że w niektóre dni nie wydano żadnej piosenki

# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
data3 <- spotify %>% 
  filter(in_spotify_playlists >= quantile(in_spotify_playlists, 0.8, na.rm=TRUE))

ggplot(data3, aes(x = mode, y = bpm)) + 
  geom_boxplot(fill = 'yellow') +
  labs(title = 'Rozkład tempa',
       x = 'Mode', y = 'bpm') 

#Rozkład tempa w obu przypadkach jest podobny, w major jednak przyjmowane są częściej większe wartości


# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

library(tidyr)

data4 <- spotify %>% 
  separate_rows(artist.s._name, sep = ", ") %>%
  mutate(streams = as.numeric(streams)) %>% 
  group_by(artist.s._name) %>% 
  summarise(total_streams = sum(streams, na.rm = TRUE)/1000000) %>% 
  arrange(-total_streams) %>% 
  head(10)

ggplot(data4, aes(y = reorder(artist.s._name, total_streams), x = total_streams)) +
  geom_col(fill = 'orange') + 
  labs(y = 'Artysta', x = 'Łączna liczba streamów w milionach', title = 'Top 10 artystów ze względu na odtworzenia')

#najwiecej odtworzen ma The Weekend, zaś najmniej BTS

# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

ggplot(spotify, aes(x = energy_., y = danceability_., color = as.character(artist_count)))+
  geom_point() + 
  labs(x = 'Energetyczność', y = 'Taneczność', title = 'Taneczność od energetyczności dla różnych liczb artystów') %>% 
  scale_color_discrete(name = 'Liczba artystów')

#w ogromnej większości piosenki były zarówno energetyczne jak i taneczne


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

data6 <- spotify %>% 
  filter(released_year %in% 2013:2023) %>% 
  group_by(released_year) %>% 
  filter(streams == max(as.numeric(streams))) %>% 
  select(released_year, track_name) %>% 
  arrange(released_year) %>% 
  View()
#dane przedstawione w ramce danych data6

# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

data7 <- spotify %>% 
  filter(released_year == 2022) %>% 
  group_by(released_month) %>% 
  summarise(sum_spotify_playlists = log(sum(in_spotify_playlists, na.rm = TRUE)),
            sum_apple_playlists = log(sum(in_apple_playlists, na.rm = TRUE))) %>% 
  mutate(month = factor(month.name, levels = month.name)) %>% 
  pivot_longer(cols = c(sum_spotify_playlists, sum_apple_playlists))

ggplot(data7, aes(x = value, y = month, group = name, fill = name)) +
  geom_col(position = 'dodge') +
  labs(x = 'Liczba piosenek w playlistach w skali logarytmicznej', y = 'Miesiąc') + 
  scale_fill_discrete(labels = c('Apple', 'Spotify'))
  
#zarówno na apple jak i spotify największa liczba  piosenek była w maju,
#spotify ma znacznie większą liczbę piosenek w  playlistach dlatego zastosowałem skalę logarytmiczną

