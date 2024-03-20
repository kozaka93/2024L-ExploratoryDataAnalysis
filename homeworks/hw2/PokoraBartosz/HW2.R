
install.packages("dplyr", "ggplot2", "ggrepel", "lubridate")
library(dplyr)
library(ggplot2)
library(ggrepel)
library(lubridate)

# Wczytanie i przygotowanie danych
spotify <- read.csv("spotify-2023.csv")
spotify <- spotify %>%
  mutate(streams = as.numeric(streams))

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

# wykres 1 (rozkład odtworzeń względem liczby wykonawców, słupkowy)
spotify %>%
  filter(released_year == 2023) %>%
  filter(released_month %in% c(1, 2, 3)) %>%
  group_by(artist_count) %>%
  summarise(sum_streams = sum(streams, na.rm = TRUE)) %>%
  ggplot(aes(x = artist_count, y = sum_streams)) +
  geom_col() +
  labs(title = "Suma odtworzeń piosenek opublikowanych w pierwszym kwartale \nroku 2023 w podziale na liczbę wykonawców",
       x = "Liczba wykonawców",
       y = "Suma odtworzeń")

# wykres 2 (rozkład odtworzeń dla danej liczby wykonawców, pudełkowy)
spotify %>%
  filter(released_year == 2023) %>%
  filter(released_month %in% c(1, 2, 3)) %>%
  ggplot(aes(x = artist_count, y = streams, group = artist_count)) +
  geom_boxplot() +
  labs(title = "Rozkład odtworzeń piosenek opublikowanych w pierwszym kwartale \nroku 2023 w zależności od liczby wykonawców",
       x = "Liczba wykonawców",
       y = "Odtworzenia") +
  theme(plot.title = element_text(size = 10))

# Komentarz: 
# Nie ma w danych piosenek z tego okresu z więcej niż trzema wykonawcami.
# Piosenki z trzema wykonawcami mają zdecydowanie mniej odtworzeń niż te z jednym lub dwoma.
# Piosenki z dwoma wykonawcami są na ogół najpopularniejsze.


# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

spotify %>%
  filter(released_year %in% 2019:2022) %>%
  mutate(
    released_day_of_week = wday(
      as.Date(paste(released_year, released_month, released_day, sep = "-")),
      label = TRUE, abbr = TRUE, week_start = 1)
  ) %>%
  group_by(released_year, released_day_of_week) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = released_day_of_week, y = count, fill = released_day_of_week)) +
  geom_col(position = "dodge") +
  facet_wrap(~released_year) +
  labs(title = "Liczba wypuszczonych piosenek względem dnia tygodnia \ndla poszczególnych lat między 2019 a 2022",
       x = "Dzień tygodnia",
       y = "Liczba piosenek") +
  theme(legend.position = "none", plot.title = element_text(size = 10))

# Komentarz:
# We wszystkich latach zdecydowanie najwięcej piosenek wyszło w piątki


# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

spotify %>%
  filter(streams/in_spotify_playlists > quantile(streams/in_spotify_playlists, 0.8, na.rm = TRUE)) %>%
  ggplot(aes(x = bpm, fill = mode)) +
  geom_density(alpha = 0.5) +
  labs(title = "Rozkład tempa piosenek w 20% najczęściej odtwarzanych \nw przeliczeniu na liczbę playlist spotify",
       x = "BPM",
       y = "Gęstość") +
  theme(plot.title = element_text(size = 10))

# Komentarz:
# Dla wyższych bpm (koło 150-190) więcej jest piosenek w skali Major, niż Minor
# Między 120 a 150 bpm więcej jest piosenek w skali Minor


# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

spotify %>%
  group_by(artist.s._name) %>%
  summarise(sum_streams = sum(streams, na.rm = TRUE)) %>%
  arrange(desc(sum_streams)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(artist.s._name, sum_streams), y = sum_streams)) +
  geom_col() +
  labs(title = "Top 10 artystów z największą liczbą odtworzeń",
       x = "Artysta",
       y = "Suma odtworzeń") +
  coord_flip()

# Komentarz:
# The Weeknd GOAT


# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>%
  ggplot(aes(x = danceability_., y = energy_., color = as.factor(artist_count))) +
  geom_point() +
  facet_wrap(~artist_count) +
  labs(title = "Zależność energetyczności od taneczności piosenek dla poszczególnych liczb artystów",
       x = "Taneczność",
       y = "Energetyczność") +
  theme(legend.position = "none", plot.title = element_text(size = 10))

# Komentarz:
# Nie zależy.


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka była najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

spotify %>%
  filter(released_year %in% 2013:2023) %>%
  group_by(released_year) %>%
  filter(streams == max(streams, na.rm = TRUE)) %>%
  ggplot(aes(x = as.factor(released_year), y = streams, label = track_name)) +
  geom_col(fill = "lightgreen") +
  geom_label_repel() +
  ylim(0, 4.2e9) +
  labs(title = "Najpopularniejsza piosenka wydana w danym roku dla lat 2013 - 2023",
       x = "Rok",
       y = "Liczba odtworzeń") +
  theme(plot.title = element_text(size = 10))

# Komentarz:
# Właściwie to my nie mamy danych, żeby odpowiedzieć na to pytanie.
# Nie ma informacji o liczbie odtworzeń danego utworu w danym roku.
# Możemy jedynie określić, która z piosenek wydana w danym roku
# miała największą liczbę odtworzeń w ogóle w momencie stworzenia tabelki z danymi.
# Stąd na przykład wynikają pewne fikołki w wynikach, takie jak Blinding Lights jako
# "najpopularniejsza" piosenka 2019 roku, co jest oczywistą nieprawdą.
# Jako że została wydana dopiero pod koniec listopada, nie miała szans na zdobycie takiej
# liczby odtworzeń jak inne hity z tego roku. Ogromną popularność zyskała dopiero później.


# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

spotify %>%
  filter(released_year == 2022) %>%
  group_by(released_month) %>%
  summarise(sum_in_spotify_playlists = sum(in_spotify_playlists, na.rm = TRUE),
            sum_in_apple_playlists = sum(in_apple_playlists, na.rm = TRUE)) %>%
  ggplot(aes(x = as.factor(released_month), y = sum_in_spotify_playlists, color = "Spotify", group = 1)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y = sum_in_apple_playlists, color = "Apple")) +
  geom_point(aes(y = sum_in_apple_playlists, color = "Apple")) +
  labs(title = "Suma piosenek na playlistach spotify i apple w 2022 roku",
       x = "Miesiąc",
       y = "Suma piosenek (skala logarytmiczna)") +
  theme(legend.position = "bottom", plot.title = element_text(size = 10)) +
  scale_y_log10()

# Komentarz:
# Suma piosenek na playlistach apple zmienia się z miesiąca na miesiąc
# w bardzo podobny sposób co na suma piosenek na playlistach spotify.
# Piosenek na playlistach apple jest jednak ogólnie zdecydowanie mniej.

