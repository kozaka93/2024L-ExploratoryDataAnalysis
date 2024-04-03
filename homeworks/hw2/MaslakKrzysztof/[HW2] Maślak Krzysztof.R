spotify <- read.csv("C:\\Users\\PC\\Desktop\\byk\\spotify-2023.csv")
View(spotify)
install.packages("tidyr")

library(ggplot2)
library(tidyverse)
library(dplyr)


remove.packages("rlang")
remove.packages("tidyr")

install.packages("rlang")
install.packages("tidyr")

library(rlang)
library(tidyr)

spotify$streams <- as.numeric(spotify$streams)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>% 
  filter(released_year == 2023, released_month <= 3) %>% 
  mutate(streams = as.numeric(streams)) %>% 
  ggplot(aes(x = as.factor(artist_count), y = streams)) +
  labs(x = "liczba artystów", y = "Rozkład liczby odtworzeń") +
  geom_boxplot()

# Najczęściej słuchane piosenki w tej kategorii to te wykonywane przez dwóch artystów.
           
# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

spotify %>% 
  mutate(day_of_week = weekdays(as.Date(paste(spotify$released_year, spotify$released_month, spotify$released_day, sep="-")))) %>% 
  filter(2019 <= released_year, released_year <= 2022) %>% 
  mutate(day_of_week = factor(day_of_week, levels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela"))) %>% 
  ggplot(aes(x = day_of_week)) +
  geom_bar() +
  facet_wrap(~released_year, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme_bw() +
  labs(x = "Dzień Tygodnia", y = "Liczba Piosenek", 
       title="Liczba wypuszczonych piosenek danego dnia tygodnia w latach 2019-2022")

# Najczęstszym dniem wypuszczania nowych piosenek jest piątek. Rozkład jest podobny, przy czym
# w 2022 zostało wypuszczonych zdecydowanie najwięcej piosenek.

# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

spotify %>% 
  filter(track_name != "Love Grows (Where My Rosemary Goes)") %>% 
  mutate(rate = as.numeric(streams) / in_spotify_playlists) %>% 
  filter(rate >= quantile(rate, 0.8)) %>% 
  ggplot(aes(x = bpm, color = mode)) +
  geom_density()

# Tempo mode Minor jest trochę wolniejsze aniżeli mode Major.

# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

spotify %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarize(liczba = sum(streams, na.rm = TRUE)) %>% 
  arrange(-liczba) %>% 
  top_n(10) %>% 
  mutate(artysta = forcats::fct_reorder(artist.s._name, -liczba)) %>% 
  ggplot(aes(x = artysta, y = liczba)) +
  geom_col(fill = "darkblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Liczba odtworzeń piosenek najpopularniejszych artystów", x = "Artysta", y = "Liczba odtworzeń", subtitle = "W serwisie Spotify" )
  
# Najwięcej odtworzeń mieli: The Weeknd, Taylor Swift i Ed Sheeran


# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od taneczności patrząc przez pryzmat liczby artystów?

ggplot(spotify, aes(x = danceability_., y = energy_.)) +
  geom_point(position = "jitter") +
  facet_wrap(~artist_count, nrow = 2) +
  xlim(0, 100) +
  ylim(0, 100) +
  labs(x="Taneczność w %", y="Energetyczność w %", 
       title="Zależność energetyczności od taneczności dla danej liczby artystów") +
  theme_bw()

# Wraz ze wzrostem liczby artystów punkty przesuwają się w prawy górny róg, 
# więc wraz ze wzrostem liczby artystów rośnie taneczność i energia utworów.


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

spotify %>% 
  filter(released_year >= 2013, released_year <= 2023) %>% 
  group_by(released_year) %>% 
  top_n(1, streams) %>% 
  mutate(year = forcats::fct_reorder(as.factor(released_year), -streams)) %>% 
  ggplot(aes(x = year, y = streams)) + 
  geom_col(fill = "blue") +
  geom_text(aes(label = track_name), hjust = 1.5, angle = 90, color = "white") + 
  labs(x = "Rok Wydania", y = "Liczba Odtworzeń", title = "Najbardziej popularne piosenki w danym roku") +
  theme_dark()

# Najbardziej populane piosenki w każdym roku są takie jak na wykresie.


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



