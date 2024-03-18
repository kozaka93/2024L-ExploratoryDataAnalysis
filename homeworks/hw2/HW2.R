spotify <- read.csv("spotify-2023.csv")
library(tidyr)
library(dplyr)
library(ggplot2)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>% 
  filter(released_year == 2023 & (released_month == 1|released_month == 2|released_month == 3)) %>% 
  group_by(artist_count) %>% 
  mutate(streams = as.numeric(streams)) %>% 
  ggplot(aes(x = as.character(artist_count), y = streams))+
  labs(x = "Liczba wykonawców",
       y = "Liczba odtworzeń") +
  geom_boxplot()


# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

library(lubridate)
spotify %>% 
  filter(released_year %in% c(2019,2020,2021,2022)) %>%
  mutate(day_of_week = wday(ymd(paste(released_year, released_month, released_day, sep="-")), label = TRUE, week_start = 1)) %>% 
  group_by(day_of_week, released_year) %>% 
  mutate(streams = as.numeric(streams)) %>% 
  summarise(n = sum(streams, na.rm = TRUE)) %>% 
  ggplot(aes(x = day_of_week, y = n, fill = factor(released_year))) +
  geom_col(position = "dodge") +
  labs(x = "Dzień tygodnia",
       y = "Liczba odsłuchań") +
  scale_fill_discrete(name = "Rok") 

# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

spotify %>% 
  filter(in_spotify_playlists >= quantile(in_spotify_playlists, 0.8)) %>% 
  ggplot(aes(x = mode, y = bpm)) +
  geom_boxplot()


# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

spotify %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  mutate(streams = as.numeric(streams)) %>% 
  summarise(n = sum(streams, na.rm = TRUE)) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  ggplot(aes(y =  reorder(artist.s._name, n), x = n)) +
  geom_col()

#założyłem, że słowo "(osobno)"  w poleceniu oznacza, że badamy tylko piosenki wykonane przez 1 artyste. 

# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?


spotify %>% 
  ggplot(aes(y = danceability_., x = energy_., color = as.character(artist_count)))+
  geom_point() +
  labs(y = "taneczność", x = "energetyczność")+
  scale_color_discrete(name="Liczba artystów")

# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

spotify %>% 
  filter(2013 <= released_year & released_year <= 2023) %>% 
  group_by(released_year) %>% 
  mutate(streams = as.numeric(streams)) %>% 
  filter(streams == max(streams)) %>% 
  select(released_year, track_name, streams) %>% 
  arrange(desc(-released_year)) %>% 
  View()

#w tym przypadku dobrym zwizualizowaniem danych moim skromnym zdaniem będzie stara dobra tabelka

# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

spotify %>% 
  filter(released_year == 2022) %>% 
  group_by(released_month) %>% 
  summarise(sum_apple = log(sum(in_apple_playlists, na.rm = TRUE)), sum_spotify = log(sum(in_spotify_playlists, na.rm = TRUE))) %>% 
  mutate(released_month = factor(month.name, levels = month.name)) %>% 
  pivot_longer(cols = c(sum_apple, sum_spotify)) %>% 
  ggplot(aes(y = released_month, x = value, group = name, fill = name))+
  geom_col(position = "dodge")+
  labs(x = "logarytm z liczby piosenek na playlistach",
       y = "Miesiąc")+
  scale_fill_discrete(labels = c("Apple", "Spotify"))

#skala liniowa w tym przypadku była wielce nieczytelna, więc skorzystałem z logarytmicznej






