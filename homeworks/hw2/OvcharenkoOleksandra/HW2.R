spotify <- read.csv("spotify-2023.csv")
library(stringr)
library(tidyverse)
library(lubridate)

options(scipen = 12)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>% 
  filter(released_year == 2023 & released_month < 4 & 
           str_detect(streams, "^[:digit:]+$")) %>% 
  ggplot(aes(x = as.numeric(streams)/1000000, y = as.factor(artist_count))) +
  geom_violin(fill = "lightgreen", draw_quantiles = 1/2) + 
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1500)) +
  coord_flip() +
  labs(title = "Rozklad liczby odtworzeń piosenek opublikowanych w pierwszym 
       kwartale 2023 roku w zależności od liczby wykonawców", 
       x = "Liczba odtworzeń w milionach", y = "Liczba wykonawców") +
  theme_minimal()

# Odp.: Widzimy, że rozkład liczby odtworzeń piosenek jest o wiele więcej "rozciągnięty"
# dla 1 i 2 wykonawców, niż dla 3, ponieważ większość piosenek ma nie więcej niż 
# 2 wykonawców. 


# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

spotify %>% 
  filter(released_year >= 2019 & released_year <= 2022) %>% 
  mutate(day_of_week = wday(make_date(released_year, released_month, released_day),
                            label = TRUE)) %>% 
  ggplot(aes(x = as.factor(released_year), fill = day_of_week)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Rozklad liczby wypuszczonych piosenek względem dnia tygodnia w 
       latach 2019-2022", x = "Rok", y = "Liczba wypuszczonych piosenek", 
       fill = "Dzień tygodnia") +
  theme_minimal()

# Odp.: Możemy zauważyć, że najwięcej piosenek publikuje się w piątek, zaś najmniej -
# w sobotę. Prawdopodobnie jest to związane z tym, że Spotify resetuje swoje 
# cotygodniowe rankingi i algorytmy w piątki.


# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

spotify %>% 
  arrange(-in_spotify_playlists) %>% 
  slice_head(prop = 0.2) %>% 
  ggplot(aes(x = bpm, y = mode)) +
  geom_boxplot(fill = "pink") +
  scale_x_continuous(expand = c(0, 0), limits = c(50, 220)) +
  coord_flip() +
  labs(title = "Rozklad tempa względem skali dla 20% najczęściej odtwarzanych piosenek 
       (w przeliczeniu na liczbę playlist spotify)", x = "Tempo (w uderzeniach na minutę)",
       y = "Skala") +
  theme_minimal()

# Odp.: Widzimy, że rozkład tempa dla 20% najczęściej odtwarzanych piosenek jest 
# nieco bardziej rozciągnięty dla skali Major, niż dla skali Minor. Zatem piosenki 
# w skali Major są bardziej urozmaicone względem tempa.


# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

spotify %>% 
  filter(str_detect(streams, "^[:digit:]+$")) %>% 
  group_by(artist.s._name) %>% 
  summarise(total_streams = sum(as.numeric(streams))) %>% 
  arrange(-total_streams) %>% 
  slice_head(n = 10) %>% 
  mutate(artist.s._name = fct_reorder(artist.s._name, total_streams)) %>% 
  ggplot(aes(x = artist.s._name, y = total_streams/1000000)) +
  geom_col(fill = "burlywood", width = 0.7) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15000)) +
  coord_flip() +
  labs(title = "Top 10 artystów, którzy mają najwięcej odtworzeń swoich piosenek", 
       x = "Artyści", y = "Liczba odtworzeń piosenek (w milionach)") +
  theme_minimal()

# Odp.: Widzimy, że 10 artystów, którzy mają najwięcej odtworzeń swoich piosenek na
# Spotify, to The Weekend, Taylor Swift, Ed Sheeran, Harry Styles, Bad Bunny, Olivia
# Rodrigo, Eminem, Bruno Mars, Arctic Monkeys i Imagine Dragons.


# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>% 
  ggplot(aes(x = danceability_., y = energy_., color = as.factor(artist_count))) +
  geom_point() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  labs(title = "Zależność energetyczności piosenki od jej tanecznośći (ze względu na 
       liczbę artystów)", x = "Taneczność (w procentach)", y = "Energetyczność 
       (w procentach)", color = "Liczba artystów") +
  theme_minimal()

# Odp.: Trudno zauważyć dokładną korelację pomiędzy energetycznością a tanecznością
# piosenki. Nie wygląda też na to, żeby liczba artystów miała znaczący wpływ na ten 
# związek.


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

spotify %>% 
  filter(released_year >= 2013 & released_year <= 2023 & 
           str_detect(streams, "^[:digit:]+$")) %>% 
  group_by(released_year) %>% 
  filter(as.numeric(streams) == max(as.numeric(streams))) %>% 
  ggplot(aes(x = as.factor(released_year), y = as.numeric(streams)/1000000, 
             fill = track_name)) +
  geom_col(width = 0.6) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4000)) +
  labs(title = "Najbardziej popularne piosenki w latach 2013-2023", x = "Rok", 
       y = "Liczba odtworzeń piosenki (w milionach)", fill = "Nazwa piosenki") +
  theme_minimal()

# Odp.: Najbardziej popularnymi piosenkami w latach 2013-2023 były, odpowiednio, 
# Take Me To Church, Thinking Out Loud, Love Yourself, One Dance, Shape of You, 
# Someone You Loved, Blinding Lights, Heat Waves, STAY, As It Was i Flowers.


# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

spotify %>% 
  filter(released_year == 2022) %>% 
  group_by(released_month) %>% 
  reframe(total_spotify_playlists = sum(in_spotify_playlists),
          total_apple_playlists = sum(in_apple_playlists)) %>% 
  ggplot(aes(x = total_apple_playlists, y = total_spotify_playlists, 
             color = as.factor(released_month))) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 3000)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 210000)) +
  labs(title = "Zależność pomiędzy sumą piosenek na playlistach na spotify a apple 
       w roku 2022 (patrząc po miesiącach)", x = "Suma piosenek na playlistach apple", 
       y = "Suma piosenek na playlistach spotify", color = "Miesiąc") +
  theme_minimal()

# Odp.: Widzimy, że na ogół im więcej piosenek jest w danym miesiącu na playlistach 
# na spotify, tym też więcej piosenek w tym miesiącu jest na playlistach apple. 
# W 2022 roku najwięcej piosenek na playlistach na spotify i apple było w maju.
