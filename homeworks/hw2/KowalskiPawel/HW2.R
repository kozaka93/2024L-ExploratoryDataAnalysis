spotify <- read.csv("C:/Users/kowal/Desktop/Wstep do eksploracyji/spotify-2023.csv", encoding = "UTF-8")
library(dplyr)
library(ggplot2)
library(tidyr)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?


spotify %>%
  filter(released_year == 2023 & released_month >=1 & released_month <= 3) %>% 
  mutate(streams_numeric = as.numeric(streams)/ 10^6) %>%
  ggplot(aes(x = as.factor(artist_count), y = streams_numeric)) +
  labs(x = "Liczba artystów",
       y = "Liczba odtworzeń (w mln)",
       title = "Rozkład liczby odtworzeń piosenek w pierwszym kwartale 2023 roku") +
  geom_boxplot()

# W każdym pzypadku mediana odtworzeń jest na podobnym poziomie. Zauważmy jednak, że w przypadku
# dwóch artystów 3 kwartyl liczby odtworzeń ma zdecydowanie największą wartość. Warto też dodać, 
# że w przypadku jednego artysty mamy do czynienia z piosenką odtwarzaną najwięcej razy. (Sprawdziłem, że jest to Miley Cyrus - "Flowers")


# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

spotify %>% 
  filter(released_year >= 2019 & released_year <= 2022) %>%
  mutate(released_date = as.Date(paste(released_year, released_month, released_day, sep = "-"))) %>%
  mutate(day_of_the_week = weekdays(released_date)) %>% 
  group_by(released_year, day_of_the_week) %>%
  summarise(n = n()) %>%
  ggplot(aes(y = factor(day_of_the_week, levels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela")) , x = n)) +
  geom_col() +
  labs(x = "Liczba wypuszczonych piosenek",
       y = "Dzień tygodnia",
       title = "Rozkład liczby wypuszczonych piosenek względem dnia tygodnia",
       subtitle = "lata 2019 - 2022") +
  facet_wrap(~released_year)
  
# W latach 2019-2022 najwięcej piosenek było wypuszczanych w piątki. Można też zauważyć, że w 2022 roku
# mieliśmy do czynienia z największą liczbą piosenek.




# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

spotify %>% 
  arrange(desc(in_spotify_playlists)) %>%
  head(round(0.2*nrow((spotify)))) %>% 
  ggplot(aes(x = mode, y = bpm)) +
  geom_boxplot() +
  labs(title = "Rozkład tempa względem skali",
       subtitle = "dla 20% najczęściej odtwarzanych piosenek",
       y = "Tempo",
       x = "Skala")

# Dla skal "Major" i "Minor" mediana tempa jest podobna. W przypadku skali "Major" występuje
# większa różnica między trzecim a pierwszym kwartylem. "Major" osiąga rownież większe maksymalne tempo
# i mniejsze najmniejsze tempo niż "Minor".



# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

# Nie byłem pewien czy artyści osobno to tylko te wiersze, gdzie artist_count==1, 
# wiec piosenki gdzie było więcej artystów rozdzieliłem na osobne wiersze i traktowalem jako piosenki każdego z nich

spotify %>% 
  separate_rows(artist.s._name, sep =',') %>% 
  group_by(artist.s._name) %>% 
  summarise(arsum = sum(as.numeric(streams)) / 10^6) %>% 
  arrange(desc(arsum)) %>% 
  head(10) %>% 
  ggplot(aes(x = arsum, y = reorder(artist.s._name, arsum))) +
  geom_col() +
  labs(title = "Top 10 najczęściej słuchanych artystów",
       x = "Liczba odtworzeń (w mln)",
       y = "Artysta") +
  scale_x_continuous(expand = c(0.001, 100))


# Tutaj komentarz wydaje się być zbędny. Można jedynie zwrócić uwagę na dosyć dużą różnicę pomiędzy pierwszym
# miejscem (The Weeknd) a drugim (Bad Bunny).


# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>% 
  ggplot(aes(x = danceability_., y = energy_.)) +
  geom_point() +
  labs(title = "Zależność energetyczności od taneczności piosenek w zależności od liczby artystów",
       x = "Taneczność (w %)",
       y = "Energetyczność (w %)") +
  facet_wrap(~artist_count, nrow = 2) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100))

# W przypadku kiedy artystów jest czterech lub więcej próbka jest dosyć mała, ale można zauważyć,
# że piosenki są wtedy dosyć taneczne i energetyczne. W przypadku kiedy artystów jest trzech lub mniej,
# piosenek jest dużo więcej oraz wyniki są bardziej różnorodne. Większość wyników posiada jednak
# ponad 50% zarówno taneczności jak i energetyczności.





# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

spotify %>% 
  filter(released_year >= 2013 & released_year <= 2023) %>% 
  mutate(streams_mld = as.numeric(streams)/10^9) %>% 
  group_by(released_year) %>% 
  arrange(desc(streams_mld)) %>% 
  slice_head(n = 1) %>% 
  ggplot(aes(x = streams_mld, y = as.factor(released_year))) +
  geom_col(fill = "lightblue") +
  geom_text(aes(x = 0.1,label = track_name), hjust = 0) +
  labs(title = "Najbardziej popularne piosenki",
       subtitle = "lata 2013-2023",
       x = "Liczba odtworzeń (w miliardach)",
       y = "Rok") +
  scale_x_continuous(expand = c(0,0), limits = c(0,4))
  
  
# Komentarz wydaje się być zbędny. 
# Można zauważyć, że najwięcej odtworzeń ma piosenka z 2019 roku pt. "Binding Lights".



# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

spotify %>% 
  filter(released_year == 2022) %>% 
  group_by(released_month) %>% 
  summarise(n_apple = sum(in_apple_playlists != 0), n_spotify = sum(in_spotify_playlists != 0)) %>%
  ggplot() +
  geom_col(aes(x = as.numeric(released_month) - 0.2, y = n_spotify, fill = "Spotify"), width = 0.4) +
  geom_col(aes(x = as.numeric(released_month) + 0.2, y = n_apple, fill = "Apple"), width = 0.4) +
  labs(x = "miesiąc",
       y = "suma piosenek na playlistach",
       title = "Suma piosenek na playlistach na spotify oraz apple w danych miesiącach",
       subtitle = "2022 rok",
       fill = "Portal") +
  scale_x_continuous(limits = c(0.6, 12.4), breaks = 1:12) +
  scale_fill_manual(values = c("Spotify" = "darkgreen", "Apple" = "pink")) +
  scale_x_continuous(expand= c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 80))


# Na playlistach spotify w każdym miesiącu jest nie mniej piosenek co na playlistach Apple. Warto dodać,
# że jeżeli różnice w ogóle występują to są bardzo niewielkie.
  






