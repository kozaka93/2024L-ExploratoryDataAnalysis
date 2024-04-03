spotify <- read.csv("/Users/nataliagajda/Desktop/spotify-2023.csv")
View(spotify)
library(ggplot2)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>%
  filter(released_year == 2023 & released_month >= 1 & released_month <= 4) %>%
  mutate(streams_mln = as.numeric(streams) / 1000000) %>%
  ggplot(aes(x = as.factor(artist_count), y = streams_mln)) +
  geom_boxplot() +
  labs(title = "Rozkład liczby odtworzeń\nw zależności od liczby wykonawców", subtitle = "I kwartał 2023 roku",  x = "liczba wykonawców", y = "licza odtworzeń w milionach") +
  scale_y_continuous(expand = c(0.01,20)) +
  theme_bw()
  
# Można zauważyć, że mimo, że największa mediana liczby odtworzeń jest dla liczby artystów równej 2 (choć i tak jest dosyć bliska pozostałym), 
# to największą liczbę odtworzeń ma pojedynczy artysta. Dodatkowo, pojedynczy artyści mają najwięcej piosenek "wyróżniających się"
#liczbą odtworzeń (przeważająco większa co do reszty) ale również tworzą piosenki z najmniejszą liczbą odsłuchań. Najmniejszy "rozstrzał" 
#(tzn. różnica w liczbie odtworzeń) występuje natomiast dla liczby artystów równej 3 (analogicznie, największy dla 2, czyli jest tam największe
# zróżnicowanie co do liczby odsłuchań)

# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

spotify %>%
  filter(released_year <= 2022 & released_year >= 2019) %>%
  mutate(datemerged = as.Date(paste(released_year, released_month, released_day, sep = "-"))) %>%
  mutate(dayofweek = factor(weekdays(datemerged), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
  group_by(released_year, dayofweek) %>%
  summarise(howmanysongs = n()) %>%
  ggplot(aes(x = dayofweek , y = howmanysongs)) + geom_col() + 
  facet_wrap(~released_year, scales = "free_x") +
  scale_x_discrete(guide = guide_axis(title = "Dzień tygodnia", angle = 90), limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Rozkład liczby wypuszczonych piosenek\nwzględem dnia tygodnia",subtitle = "Lata 2019-2022",  y = "Liczba wypuszczonych piosenek")

# Wnioski bardzo oczywiste, najwięcej piosenek jest wypuszczanych w piątki, następnie w czwartki.
# Ciężko wskazać dalsze zależności, dla każdego rozkład wygląda troche inaczej.


# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

spotify %>%
  filter(in_spotify_playlists >= quantile(in_spotify_playlists, 0.8)) %>%
  ggplot(aes(x = mode, y = bpm)) + geom_boxplot() +
  labs(title = "Rozkład tempa względem skali", subtitle = "dla piosenek, które są w 20% najczęściej odtwarzanych\nze względu na liczbę playlist",
       x = "Skala", y = "Rozkład tempa w bpm") +
    scale_y_continuous(expand = c(0.48,0)) +
  theme_bw()

# Bpm osiąga zarówno większe jak i mniejsze wartości dla skali Major (niż dla skali Minor). 
# Skale Major charakteryzuje również większe zróżnicowanie wartości bpm. 
# Mediana jest jednak dla obu bardzo podobna.


# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

library(tidyr)
spotify %>%
  separate_rows(artist.s._name, sep = ",") %>%
  group_by(artist.s._name) %>%
  summarise(SumForArtist = sum(as.numeric(streams)) / 1000000) %>%
  arrange(-SumForArtist) %>%
  head(10) %>%
  ggplot(aes(x = SumForArtist, y = reorder(artist.s._name, SumForArtist))) +
  geom_col() +
  scale_y_discrete(guide = guide_axis(title = "Artysta", angle = 0)) +
  labs(title = "Top 10 najczęściej słuchanych artystów", x = "Liczba odtworzeń w milionach") + 
  scale_x_continuous(expand = c(0.001,100)) + 
  theme_bw()

# Najczęściej słuchanym artystą dla naszych danych jest The Weeknd, na wykresie
# możemy zaobserwować dosyć dużą różnicę dla artystów znajdujących się nawet 
# w Top 10. The Weeknd zdecydowanie wyprzedza resztę.





# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>%
  ggplot(aes(y = energy_., x = danceability_.)) + geom_point(size = 0.72) +
  labs(color = "Liczba artystów", x = 'Taneczność', y = "Energetyczność", title = "Wykres zależności energetyczności od taneczności", subtitle = "gdzie tytuły wykresów oznaczają liczbę artystów") +
  theme_bw() +
  facet_wrap(~artist_count, nrow = 2) +
  xlim(0,100)

# Pierwsza obserwacja to oczywiście zróżnicowanie, malejąca ilość piosenek wraz z rosnącą ilością artystów
# Idzie za tym również zróżnicowanie zależności energetyczności od taneczności:
# im mniej piosenek tym bardziej są one "skoncentrowane".
# Może to świadczyć po prostu o różnorodności piosenek, tych których jest najwięcej 
# są najbardziej różnorodne więc mają najwieksze żróżnicowanie w zależności (energetyczności od taneczności)
# Piosenki z większą liczbą artystów są natomiast bardziej taneczne i energetyczne.
  


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

spotify %>%
  filter(released_year >= 2013 & released_year <= 2023) %>%
  mutate(streams_numeric = as.numeric(streams) / 1000000000) %>%
  group_by(released_year) %>%
  arrange(-streams_numeric) %>%
  slice_head(n = 1) %>%
  ggplot(aes(y = as.factor(released_year), x = streams_numeric)) +
  geom_col(fill = "lightblue") +
  scale_y_discrete(guide = guide_axis(title = "Piosenka i rok", angle = 0)) +
  labs(title = "Najbardziej popularne piosenki ze względu na liczbe odtworzeń", subtitle = "2013-2023", x = "Liczba odtworzeń w miliardach") +
  scale_x_continuous(expand = c(0,0), limits = c(0,4)) +
  theme_bw() +
    geom_text(aes(x = 0.05, label = paste(artist.s._name, "-", track_name)), colour = "black", hjust = 0, check_overlap = TRUE)

# W sumie wszystko widać na wykresie, nie ma tu żadnej zależności, różnie piosenki 
# są najbardziej popularne w latach 2013-2023


# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

spotify %>%
  filter(released_year == 2022) %>%
  group_by(released_month) %>%
  summarise(sum_spoti = sum(in_spotify_playlists != 0), sum_apple = sum(in_apple_playlists != 0)) %>%
  ggplot() +
  geom_col(aes(x = as.numeric((released_month)) + 0.15, y = sum_spoti, fill = "Spotify"), width = 0.2) +
  geom_col(aes(x = as.numeric((released_month)) - 0.15, y = sum_apple, fill = "Apple"), width = 0.2) +
  labs(title = "Zależności pomiędzy sumą piosenek na playlistach", fill = "Portal", y = "Suma piosenek", x = "Miesiąc", subtitle = "Rok 2022") +
  scale_x_continuous(limits = c(0.75,12.25), breaks = 1:12) +
  scale_fill_manual(values = c("Spotify" = "darkgreen", "Apple" = "purple")) +
  theme_bw() + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 80)) 

# Można zauważyć skok sumy piosenek na playlistach wydawanych w 5 miesiącu.
# Dodatkowo na playlistach Spotify jest więcej bądź tyle samo piosenek co na playlistach Apple. 
  



