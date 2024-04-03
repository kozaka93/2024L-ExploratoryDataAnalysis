spotify <- read.csv("spotify-2023.csv")
library(dplyr)
library(ggplot2)
# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>% 
  filter(released_month < 4, released_year == 2023) %>% 
  ggplot(aes(x = as.factor(artist_count), y = as.numeric(streams))) +
  geom_boxplot(fill = "gray") +
  labs(title = "Rozkład liczby odtworzeń piosenek w zależności od liczby wykonawców",
       subtitle = "Wydanych w 2023 roku", x = "Liczba wykonawców", y = "Liczba odtworzeń")

# Średnio piosenki mające 2 wykonawców miały najwięcej odtworzeń. Ponadto zdecydowanie więcej utworów z 2 wykonawcami miało odtworzeń
# (chodzi o to, że 3 kwartyl jest zdecydowanie największy przy liczbie wykonawców równej 2). Ogólnie najsłabiej wypadały piosenki
# z 3 wykonawcami

# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?
install.packages("lubridate")
library(lubridate)

spotify %>% 
  filter(released_year > 2018, released_year < 2023) %>% 
  mutate(date = paste(released_day, released_month, released_year, sep = "/")) %>% 
  mutate(date = as.Date(date, "%d/%m/%y"), weekday = wday(date, week_start = 1)) %>% 
  ggplot(aes(x = weekday)) +
  geom_bar(colour = "black", fill = "green4", linewidth = 1)+
  facet_wrap(~released_year, scales = "free") +
  labs(title = "Liczba wypuszczonych piosenek w zależności od dnia tygodnia", 
       subtitle = "W latach 2019-2022", x = "Dzień tygodnia", y = "Liczba piosenek")

# Ogólnie z roku na rok wypuszczane było co raz więcej piosenek ze znacznym przeskokiem z roku 2021 na 2022. 
# Poza anomalicznym rokiem 2019 większość piosenek wypuszczana była w środku tygodnia (środa - piątek)

# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

spotify %>% 
  filter(in_spotify_playlists >= quantile(in_spotify_playlists, 0.8)) %>% 
  ggplot(aes(x = mode, y = bpm)) +
  geom_violin(linewidth = 1, fill = "lightblue") +
  stat_summary(fun = "mean",
               geom = "point",
               size = 3,
               color = "darkblue") +
  labs(title = "Rozkład tempa względem skali", 
       subtitle = "Dla piosenek, które są w 20% najczęściej odtwarzanych",
       y = "Tempo (BPM)", x = "Rodzaj skali")

# Utwory w skali durowej (major) mają większą rozpiętość jeśli chodzi o ich tempo niż te w skali molowej (minor). 
# Średnia tempa dla obu tych skal jest dość podobna i znajduje się w okolicy 120 BPM (podobnie jak ich mediana)


# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

spotify %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarise(suma = sum(as.numeric(streams))) %>% 
  mutate(name = fct_reorder(as.factor(artist.s._name), suma)) %>% 
  slice_max(order_by = suma, n = 10) %>% 
  ggplot(aes(x = name, y = suma)) + 
  geom_col(colour = "black", fill = "gold") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Top 10 najpopularniejszych artystów",
       subtitle = "Ze względu na sumę odtworzeń", x = "Artysta", y = "Liczba odtworzeń")

# Top 10 artystów to odpowiednio (malejąco) The Weekend, Taylor Swift, Ed Sheeran, Harry Styles, Bad Bunny,
# Olivia Rodrigo, Eminem, Bruno Mars, Arctic Monkeys, Imagine Dragons

# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>% 
  ggplot(aes(x = energy_., y = danceability_.)) +
  geom_density_2d_filled(contour_var = "ndensity") + 
  facet_wrap(~as.factor(artist_count)) +
  labs(x = "Energetyczność", y = "Taneczność", 
       title = "Zależność energetyczności od taneczności w zależności od liczby artystów")

# Poza liczbą artystów > 5 (bardzo mało danych) widzimy, że przeważnie duża taneczność utworu przekłada się na jego dużą energetyczność
# Natomiast jeśli którakolwiek z tych statystyk jest niska, nie ma między nimi większych zależności (nie ma praktycznie żadnych)




# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?
install.packages("forcats")
library(forcats)

spotify %>% 
  filter(released_year < 2024, released_year > 2012) %>% 
  mutate(streams = as.numeric(streams)) %>% 
  group_by(released_year) %>% 
  slice_max(order_by = streams, n = 3) %>% 
  mutate(title = fct_reorder(track_name, streams)) %>% 
  select(title, streams, released_year) %>%
  ggplot(aes(x = title, y = streams)) +
  geom_col(colour = "black", fill = "deepskyblue2", linewidth = 0.7) +
  facet_wrap(~released_year, scales = "free") +
  scale_x_discrete(guide = guide_axis(angle = 20)) +
  labs(title = "Najpopularniejsze utwory w latach 2013-2023",
       x = "Tytuł utworu", y = "Liczba odsłuchań")
  
# W latach 2013-2023 najpopularniejsze były kolejno: Take Me To Church, Thinking Out Loud,
# Love Yourself, One Dance, Shape of You, Someone You Loved, Blinding Lights, Heat Waves,
# STAY (with Justin Bieber), As It Was, Flowers.

# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?
install.packages("reshape2")
library(reshape2)

spotify %>% 
  filter(released_year == 2022) %>% 
  mutate(is_spotify = as.numeric(in_spotify_playlists > 0),
         is_apple = as.numeric(in_apple_playlists > 0)) %>% 
  group_by(released_month) %>% 
  summarise(Spotify = sum(is_spotify), Apple = sum(is_apple)) %>% 
  melt(id.vars = 1) %>% 
  ggplot(aes(x = "", y = value)) +
  geom_col(aes(fill = variable),position = "dodge") +
  facet_wrap(~released_month, scales = "free") +
  labs(x = "", y = "Liczba utworów na playlistach", subtitle = "Dla Spotify i Apple",
       title = "Liczba utworów na playlistach w zależności od miesiąca wydania",
       fill = "Serwis")
  
# Niezależnie od miesiąca wydania, więcej piosenek jest na playlistach na Spotify (wszystkie), ale
# prawie każda z wydanych piosenek znalazła się również na playlistach na Apple (Music)





