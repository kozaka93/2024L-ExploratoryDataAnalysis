dir_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir_path)

spotify <- read.csv("spotify-2023.csv")

library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(tidyr)
library(viridis)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>% 
  filter((released_year == 2023) & (released_month %in% c(1, 2, 3))) %>%
  mutate(streams = as.numeric(streams)) %>%
  ggplot(aes(x = as.factor(artist_count), y = streams)) +
  geom_boxplot(color="black", fill="lightblue", alpha=0.3) +
  scale_y_continuous(labels = comma) +
  labs(title = "Rozkład liczby odtworzeń piosenek w zależności od liczby wykonawców",
       x = "Liczba wykonawców",
       y = "Liczba odtworzeń") +
  theme_bw()

# Wnioski:
# Piosenki z dwoma wykonawcami mają największy rozstęp międzykwartylowy, 3 kwartyl jest
# wyraźnie wyższy niż dla piosenek z 1 lub 3 wykonawcami, co oznacza, że piosenki z dwoma
# wykonawcami są najczęściej odtwarzane.

# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

spotify %>% 
  filter(released_year %in% c(2019, 2020, 2021, 2022)) %>%
  mutate(released_day_of_week = wday(paste(released_year, released_month, released_day, sep = "-"))) %>%
  ggplot(aes(x = factor(released_day_of_week,
                        labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))) +
  facet_wrap(~released_year) +
  geom_bar(fill="lightblue", color="black") +
  labs(title = "Rozkład liczby wypuszczonych piosenek względem dnia tygodnia",
       x = "Dzień tygodnia",
       y = "Liczba piosenek") +
  theme_bw()

# Wnioski:
# Piosenki najchętniej wypuszczane są przez artystów na początku weekendu,
# czyli w piątek i sobotę. Działo się tak w każdym z lat między 2019 a 2022.

# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

spotify %>% 
  mutate(streams = as.numeric(streams)) %>%
  filter(streams > quantile(streams, 0.8, na.rm=TRUE)) %>%
  ggplot(aes(x = bpm)) +
  facet_wrap(~mode) +
  geom_density(alpha = 0.5, fill="green", color="black") +
  labs(title = "Rozkład tempa względem skali dla piosenek w 20% najczęściej odtwarzanych piosenek",
       x = "Tempo",
       y = "Gęstość") +
  theme_bw()

# Wnioski:
# Dla skali minor tempo piosenek jest bardziej skupione wokoł wartosci 100-150,
# natomiast dla skali major nieco bardziej rozproszone, oraz dwumodalne.

# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

spotify %>% 
  mutate(streams = as.numeric(streams)) %>%
  group_by(artist.s._name) %>%
  summarise(total_streams = sum(streams, na.rm = TRUE)) %>%
  arrange(desc(total_streams)) %>%
  head(10)

# Wynik:
# artist name         total_streams
# 1 The Weeknd        14185552870
# 2 Taylor Swift      14053658300
# 3 Ed Sheeran        13908947204
# 4 Harry Styles      11608645649
# 5 Bad Bunny         9997799607
# 6 Olivia Rodrigo    7442148916
# 7 Eminem            6183805596
# 8 Bruno Mars        5846920599
# 9 Arctic Monkeys    5569806731
# 10 Imagine Dragons  5272484650


# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>% 
  mutate(energy = as.numeric(energy_.),
         danceability = as.numeric(danceability_.)) %>%
  ggplot(aes(x = energy_., y = danceability, color = as.factor(artist_count))) +
  geom_point(alpha=0.7) +
  labs(title = "Zależność energetyczności od taneczności",
       x = "Energetyczność",
       y = "Taneczność") +
  scale_color_viridis(discrete = TRUE) +
  labs(color = "Liczba artystów") +
  theme_bw()

# Wnioski:
# Większa energetycznośc często idzie w parze z większą tanecznością.
# Trudno zaobserwować wpływ liczby artystów na tę zależność.

# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

spotify %>% 
  mutate(streams = as.numeric(streams)) %>%
  filter(released_year %in% 2013:2023) %>%
  group_by(released_year) %>%
  filter(streams == max(streams)) %>%
  select(released_year, track_name, artist.s._name, streams) %>% 
  arrange(released_year)

# Wynik:
# year track_name                artist name                  streams
# 2013 Take Me To Church         Hozier                       2135158446
# 2014 Thinking Out Loud         Ed Sheeran                   2280566092
# 2015 Love Yourself             Justin Bieber                2123309722
# 2016 One Dance                 Drake, WizKid, Kyla          2713922350
# 2017 Shape of You              Ed Sheeran                   3562543890
# 2018 Someone You Loved         Lewis Capaldi                2887241814
# 2019 Blinding Lights           The Weeknd                   3703895074
# 2020 Heat Waves                Glass Animals                2557975762
# 2021 STAY (with Justin Bieber) Justin Bieber, The Kid Laroi 2665343922
# 2022 As It Was                 Harry Styles                 2513188493
# 2023 Flowers                   Miley Cyrus                  1316855716

# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

spotify %>% 
  filter(released_year == 2022) %>%
  group_by(released_month) %>%
  summarise(in_spotify_playlists = sum(in_spotify_playlists, na.rm = TRUE),
            in_apple_playlists = sum(in_apple_playlists, na.rm = TRUE)) %>%
  pivot_longer(cols = c(in_spotify_playlists, in_apple_playlists)) %>%
  ggplot(aes(x = factor(released_month, labels = month.abb), 
             y = value, color = name)) +
  geom_point(size=4) +
  labs(title = "Zależność pomiędzy sumą piosenek na playlistach na spotify a apple w zależności od miesiąca, w 2022 roku",
       x = "Miesiąc",
       y = "Suma piosenek na playlistach") +
  scale_color_manual(values = c("darkgrey", "green"),
                       name = "Platforma",
                       labels = c("Apple", "Spotify")) +
  theme_bw()

# Wnioski:
# W każdym miesiącu liczba piosenek na playlistach na Spotify była znacznie wyższa niż na Apple.
