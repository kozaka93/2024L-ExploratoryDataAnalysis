spotify <- read.csv("spotify-2023.csv")
library(ggplot2)
library(dplyr)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?
spotify %>% 
  filter(released_year == 2023 & released_month <=3) %>% 
  mutate(streams = as.numeric(streams)/1000000) %>% 
  ggplot(aes(x = as.factor(artist_count), y = streams)) +
  geom_boxplot() +
  labs(x = "Liczba wykonawców",
       y = "Odtworzenia w milionach",
       title = "Rozkład liczby odtworzeń piosenek opublikowanych\n w pierwszym kwartale w roku 2023\n w zależności od liczby wykonawców")

# Mediana odtworzeń dla każdej liczby wykonawców jest zbliżona, jednak największa jest w przypadku dwóch
# artystów, zatem były ona najbardziej popularne. W tym też przypadku
# mamy doczynienia z najbardziej rozproszonymi i niesymetrycznymi danymi. Najniższa mediana widoczna
# jest w przypadku trzech artystów, w tym przypadku dane są jednak najbardziej skupione - rozstęp ćwiartkowy
# jest najmniejszy, czyli średnio utwory miały podobną liczbę odtworzeń. Wykres przedstawia również cztery
# wyniki odstające (outlinery), z czego jeden przypada na utwory z dwoma artystami i 4 na utwory z jednym
# wykonawcą z czego jeden odstaje zdecydowanie bardziej - były to najpopularniejsze utwory.


# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?
spotify %>%
  filter(released_year >= 2019 & released_year <= 2022) %>% 
  mutate(dzien_tygodnia = weekdays(as.Date(paste(released_year, released_month, released_day, sep = "-")))) %>% 
  mutate(dzien_tygodnia = factor(dzien_tygodnia, levels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela"))) %>%
  group_by(released_year, dzien_tygodnia) %>%
  summarise(ilosc_piosenek = n()) %>% 
  ggplot(aes(x = dzien_tygodnia, y = ilosc_piosenek, fill = as.factor(released_year))) +
  geom_col() +
  labs(x = "Dzień tygodnia",
       y = "Liczba wypuszczonych piosenek",
       title = "Rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla lat 2019 - 2022",
       fill = "Rok")
  
  # Na wykresie wyraźnie widać, że najwięcej piosenek było wypuszczanych w 2022 roku, niezależnie
  # od dnia tygodnia ponad połowa piosenek była z tego właśnie roku. Ponadto zdecydowanie najwięcej piosenek
  # pojawiało się w piątek, ponad 4 krotnie więcej niż w czwartek, który jest na drugim miejscu. Najmniej.
  # utworów pojawiło się w soboty oraz niedziele, niewiele więcej we we wtorki oraz poniedziałki.


# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
spotify %>% 
  mutate(streams = as.numeric(streams)) %>% 
  mutate(streams_per_playlist = streams/in_spotify_playlists) %>% 
  arrange(-streams_per_playlist) %>% 
  filter(row_number() <= floor(nrow(.)*0.2)) %>% 
  ggplot(aes(x = as.factor(mode), y = bpm)) +
  geom_boxplot() +
  labs(x = "Skala 'mode'",
       y = "Tempo piosenki (uderzenia na minutę)",
       title = "Rozkład tempa piosenki względem skali dla piosenek, które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify")
  
  # Mediana w obu skalach jest zbliżona i waha się w okolicach 125 uderzeń na minutę, jednak niewiele wyższa
  # mediana jest w  przypadku skali 'Major'. W tym przypadku mamy też doczynienia z większym rozstępem
  # ćwairtkowym - dane są bardziej rozproszone. W skali 'Minor' mamy dwie odstające obserwacje oraz widoczny
  # brak symetrii danych, z przewagą danych poniżej mediany. W skali 'Major' dane o większym tempie są
  # przyjmowane częściej.


# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?
spotify %>% 
  filter(artist_count == 1) %>% 
  mutate(streams = as.numeric(streams)) %>% 
  group_by(artist.s._name) %>% 
  summarise(a = floor(sum(streams)/1000000)) %>% 
  arrange(-a) %>% 
  head(10) %>% 
  ggplot(aes(x = forcats::fct_reorder(artist.s._name, a), y = a)) +
  geom_col(fill = "navyblue") +
  geom_text(aes(y = a, label = a), size = 2.3, vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 15, hjust = 0.5)) +
  labs(x = "Artysta",
       y = "Suma odtworzeń wszystkich\n piosenek w milionach",
       title = "Top 10 artystów, którzy osobno mają najwięszką liczbę odtworzeń wszystkich swoich piosenek w sumie")

  # Ci artyścio to kolejno (od najmniejszej do największej liczby odtworzeń):
  # Imagine Dragons, Artic Monkeys, Bruno Mars, Eminem, Olivia Rodrigo, Bad Bunny, Harry Styles, Ed Sheeran, Taylor Swift, The Weekend
  # Między pierwszymi 4 artystami róźnice nie są znacze. Również ostatnie 3 pozycje nie róźnią się bardzo. Największa róźnica
  # jest opmiędzy pomiędzy artystami na miejscach 5-7. Pierwszy artysta ma niemal 3 krotnie wiecej odtworzeń od 10. artysty.
  # 3 najlepszych artystów ma zbliżone liczby odtworzeń.


# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?
spotify %>% 
  ggplot(aes(x = energy_., y = danceability_.)) +
  geom_point(color = "navyblue") +
  geom_smooth()+
  facet_wrap(~artist_count) +
  labs(x = "Energetyczność utowru", y = "Taneczność utworu",
       title = "Zależność energetyczności od taneczności ze względu na liczbę artystów")

  # W przypadku 1 artysty dane są mocno rozproszone, ale widać że największe skupienie występuję w prawej, górnej części
  # wykresu. To oznacza, że najwięcej utworów jest "mocno" tanecznych i energetycznych, jednak rozpiętość jest duża i 
  # niemal w każdej części wykresu możemy znaleźć jakiś utwór. Zależność zdaje się być zbliżona do liniowej,
  # jadnak linia w prawej cześci wykresu się załamuje.
  # W przypadku dwóch i trzech artystów skupienie danych w prawym górnym rogu jest już znacznie widoczne. Zatem niewiele
  # jest utworów o małej taneczności i energetyczności. W obu przypadkach zależność ponownie wydaje się być zbliżona
  # do utworów z jednym artystą. W pozostalych przypadkach danych jest niewiele, jednak ponownie można
  # zauważyć tendencję do skupienie danych w prawym górnym rogu.
   


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?
spotify %>% 
  filter(released_year >= 2013 & released_year <= 2023) %>% 
  mutate(streams = as.numeric(streams)) %>% 
  group_by(released_year) %>% 
  filter(streams == max(streams)) %>% 
  mutate(streams = streams/1000000) %>% 
  ggplot(aes(x = released_year, y = streams)) +
  coord_flip() +
  geom_col(fill = "navyblue") + 
  geom_text(aes(label = track_name), size = 3.4, color = "white", hjust = 1.1, vjust = 0.25) +
  scale_x_continuous(breaks = 2013:2023) +
  labs(x = "Rok",
       y = "Liczba odtworzeń w milionach",
       title = "Najbardziej popularne piosenki w poszczególnych latach")
  # A więc w kolejnych latach najpopularniejsze piosenki to: "Flower", "Love Yourself", "Take me to Church", "Thinking Out Loud", 
  # "As it was", "Heat Waves", "Stay (with Justin Bieber)", "Once Dance", "Someone You Loved", "Shape of You", "Blinding Lights".
  # Najpopularniejszą z tych piosenek jest "Blinding Light" z 2019, a najmniej popularny utwór to "Flowers" z 2023


# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

spotify %>% 
  filter(released_year == 2022) %>% 
  group_by(released_month) %>% 
  summarise(spotifyy = sum(in_spotify_playlists)/1000, apple = sum(in_apple_playlists)/1000) %>% 
  ggplot(aes(x = released_month)) +
  geom_point(aes(y = apple, color = "Apple")) +
  geom_text(aes(y = apple, label = apple), vjust = -0.7, color = "black", size = 2) +  
  geom_point(aes(y = spotifyy, color = "Spotify"), size = 3) +
  geom_text(aes(y = spotifyy + 10, label = spotifyy), vjust = -0.5, color = "darkgreen", size = 2) + 
  scale_x_continuous(breaks = 1:12) + 
  scale_colour_manual(values = c("black", "green")) +
  labs(x = "Miesiąc",
       y = "Liczba odtworzeń w tysiącach",
       title = "Suma piosenek na playlistach na spotify i apple w 2022 roku",
       color = "Platforma")
  
  # W każdym z miesięcy apple ma zdecydowaną przewagę, mając za każdym razem kilkadziesiąt lub kilkaset razy
  # więcej piosenek. Zdecydowanie wyróźnia się maj, w którym liczba piosenek na obu platformach znacznie
  # znacznie wzrosła.
