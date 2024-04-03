library(dplyr)
library(ggplot2)
spotify <- read.csv("C:/Users/Maks/Desktop/MiAD/Sem 4/WDED/Pd 2/spotify-2023.csv")
head(spotify)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?
p <- spotify %>% filter(released_year == 2023 & released_month <= 3) %>% 
  select(track_name, artist_count, streams) %>%
  ggplot(aes(x = as.character(artist_count), y = as.integer(streams))) + geom_boxplot()

p + ggtitle("rozkład liczby odtworzeń piosenek opublikowanych w roku 2023
w pierwszym kwartale w zależności od liczby wykonawców") +
  xlab("Ilość wykonawców") + ylab("LIczba odtworzeń")


  # Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?
p <- spotify %>% filter( released_year >= 2019 & released_year <= 2022) %>%
  mutate(date = paste(released_year, released_month, released_day,sep="-")) %>% 
  mutate(day = weekdays(as.Date(date, "%Y-%m-%d")))

p %>% ggplot(aes(x = released_year, fill = day)) + geom_bar()

# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

p <- spotify %>% mutate(ratio = as.numeric(streams) / in_spotify_playlists)
p %>% filter(ratio >= quantile(p$ratio, 0.8, na.rm = TRUE)) %>% 
  ggplot(aes(x = mode, y = bpm, fill = mode)) + geom_violin() +
  ggtitle("rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?") +
  xlab("Skala") + ylab("Tempo")





# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

spotify %>%
  group_by(artist.s._name) %>%
  summarise(record_sum = sum(as.numeric(streams), na.rm = TRUE)) %>%
  top_n(10, record_sum)%>%
  ggplot(aes(x = reorder(artist.s._name, record_sum), y = record_sum)) +
  geom_bar(stat = "identity", fill="darkblue") +
  labs(title = "Top 10 Artystów według liczby odtworzeń",
       x = "Artysta",
       y = "Odtworzenia") +
  theme_minimal() + coord_flip() 





# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>%
  ggplot(aes(x = danceability_., y = energy_., color = as.character(artist_count))) + 
  geom_point() + theme_bw() +
  labs(x = "Taneczność", y = "Energetyczność", color = "Ilosc artystow",
       title = "Energetyczność od tanecznośći patrząc przez pryzmat liczby artystów") +
  theme(legend.position = "top") + facet_wrap(~artist_count, ncol = 4)


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?
spotify %>%
  filter(released_year >= 2013 & released_year <= 2023) %>%
  mutate(streams = as.integer(streams)) %>% 
  group_by(released_year) %>% top_n(1, streams) %>%
  mutate(track_name = forcats::fct_reorder(track_name, released_year)) %>%
  ggplot(aes(x = as.factor(released_year), y = streams, label = track_name)) +
  geom_col(fill = "#333399") + coord_flip() + scale_y_log10() +
geom_text(position = position_stack(vjust = 0.5), size = 5, color = "lightgrey",
          family="Times", fontface="italic")

# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

p <- spotify %>% filter(released_year == 2022) %>% group_by(released_month) %>%
  summarise(spoti = sum(in_spotify_playlists), apple = sum(in_apple_playlists)) %>% 
  mutate(Month = month.name[released_month],
         Month = forcats::fct_reorder(Month, released_month)) %>% 
  ggplot(aes(x = spoti, y = apple, color = Month)) + geom_point(size = 5)

p + ggtitle("Zależność pomiędzy sumą piosenek na playlistach na spotify a apple
patrząc po miesiącach, rok 2022") + xlab("Spotify") + ylab("Apple Music")
