options(scipen = 100, digits = 4)

library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(forcats)
library(tidyr)
library(ggimage)

spotify <- read.csv("spotify-2023.csv")


# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>% 
  filter(released_year == 2023,
         released_month <= 3) %>% 
  ggplot(aes(x = as.factor(artist_count),
             y = as.numeric(streams))) +
  geom_boxplot(fill = "#66c2a5") +  # Dodanie koloru
  labs(x = "Liczba wykonawców", y = "Liczba odtworzeń",
       title = "Rozkład liczby odtworzeń piosenek w 2023 roku (I kwartał)") +
  theme_minimal()

# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

days_order <- weekdays(as.Date(c("2024-03-18",
                                 "2024-03-19",
                                 "2024-03-20",
                                 "2024-03-21",
                                 "2024-03-22",
                                 "2024-03-23",
                                 "2024-03-24")))

spotify %>%
  filter(released_year >= 2019 & released_year <= 2022) %>% 
  mutate(day_of_week = factor(weekdays(as.Date(paste(released_year, 
                                              released_month, 
                                              released_day, 
                                              sep = "-"))),
                              levels = days_order)) %>% 
  ggplot(aes(x = day_of_week, fill = as.factor(released_year))) +
  geom_bar(position = "dodge") +
  labs(x = "Dzień miesiąca", y = "Liczba piosenek",
       title = "Rozkład liczby wypuszczonych piosenek względem dnia miesiąca (2019-2022)") +
  scale_fill_manual(values = c("2019" = "#FFA500", "2020" = "#008000", "2021" = "#0000FF", "2022" = "#FF0000")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

spotify %>% 
  filter(in_spotify_playlists >= quantile(spotify$in_spotify_playlists, probs = 0.8, na.rm = TRUE)) %>% 
  ggplot(aes(x = bpm, fill = mode)) +
  geom_density(alpha = 0.5, adjust = 0.75) +
  labs(x = "Tempo (BPM)", y = "Gęstość", fill = "Skala",
       title = "Rozkład tempa w zależności od skali dla\n 20% najczęściej odtwarzanych piosenek") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"), labels = c("molowa", "durowa")) +
  theme_minimal()

# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

spotify %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarise(total_streams = sum(as.numeric(streams), na.rm = TRUE)) %>% 
  arrange(desc(total_streams)) %>% 
  top_n(10) %>% 
  ggplot(aes(y = reorder(artist.s._name, total_streams),
             x = total_streams)) +
  geom_col(fill = "#C39DF1") +
  labs(y = "Artysta", x = "Liczba odtworzeń",
       title = "Top 10 artystów z największą liczbą odtworzeń wszystkich swoich piosenek") +
  theme_minimal()

# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>%
  ggplot(aes(x = danceability_., y = energy_., color = factor(artist_count))) +
  geom_point(alpha = 0.5) +
  facet_wrap(~artist_count, scales = "free") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_color_discrete(name = "Liczba artystów") +
  labs(x = "Taneczność (%)", y = "Energetyczność (%)",
       title = "Zależność między energetycznością a tanecznością\nw zależności od liczby artystów") +
  theme_minimal()

# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?


spotify %>%
  filter(released_year >= 2013 & released_year <= 2023) %>%
  group_by(released_year) %>%
  filter(as.numeric(streams) == max(as.numeric(streams))) %>%
  select(released_year, track_name, artist.s._name, streams) %>% 
  ggplot(aes(x = as.factor(released_year),
             y = as.numeric(streams),
             label = paste(track_name, artist.s._name, sep="\n"),
             image = paste("./images/", released_year, ".jpg", sep=""))) +
  geom_col(size = 3, fill = "lightpink") +
  geom_image(aes(y = as.numeric(streams) + 500000000), size = 0.1) +
  geom_text(vjust = -0.5, size = 2.5) +
  scale_y_continuous(limits = c(0, 5*10^9)) +
  labs(x = "Rok", y = "Liczba odtworzeń",
       title = "Najbardziej popularne piosenki w latach 2013-2023") +
  theme_minimal()

# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

spotify %>%
  filter(released_year == 2022) %>%
  group_by(released_month) %>%
  summarise(Apple = sum(in_apple_playlists, na.rm = TRUE),
            Spotify = sum(in_spotify_playlists, na.rm = TRUE)) %>% 
  pivot_longer(!released_month, names_to="platform", values_to="sum") %>% 
  ggplot(aes(x = factor(released_month),
             y = sum,
             fill = platform)) +
  geom_col(position = "dodge") +
  labs(x = "Miesiąc", y = "Suma piosenek na playlistach", 
       title = "Zależność sumy piosenek na playlistach Spotify i Apple w 2022 roku",
       fill = "Platforma") +
  scale_x_discrete(labels = c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec", 
                              "Lipiec", "Sierpień", "Wrzesień", "Październik", "Listopad", "Grudzień")) +
  theme_minimal()
