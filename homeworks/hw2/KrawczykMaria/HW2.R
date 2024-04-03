spotify <- read.csv("spotify-2023.csv")
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

options(scipen = 10)

spotify %>% 
  filter(released_year == 2023) %>%
  filter(released_month == 1 | released_month == 2 | released_month == 3) %>% 
  ggplot(aes(x = as.factor(artist_count), y = as.numeric(streams))) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(title = "Zależność liczby odtworzeń na spotify od liczby wykonawców", 
       subtitle = "styczen - marzec 2023", x = "Liczba wykonawców", y = "Średnia liczba odtworzeń") -> zad1

zad1

# odp. Widzimy, że dla 1,2 jak i 3 wykonawców mediana średniej liczby odtworzeń ma podobną wartość, 
# natomiast różny jest rozrzut tych wartośći. Dla dwóch wykonawców rozrzut jest największy, choć dla jednego też
# występuje jedna dana bardzo odstająca od pozostałych, natomiast dla 3 wykonawców dane są bardzo skondenswoane i wszystkie
# bliskie medianie



# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

spotify %>% 
  filter(released_year >= 2019, released_year <= 2022) %>%
  mutate(date = as.Date(paste(released_year,released_month,released_day, sep = "-")),
            weekday = factor(weekdays(date),levels = c("poniedziałek", "wtorek","środa", "czwartek",
                                                       "piątek", "sobota", "niedziela"))) %>% 
  ggplot(aes(weekday)) +
  geom_bar(fill = "darkgreen", color = "darkgreen") +
  facet_wrap(~released_year, scales = "free_y") +
  theme(strip.background = element_rect(color = "black", fill = "lightgreen"))+
  labs(title = "Rozklad liczby wypuszczonych piosenek w zależnosci od dnia tygodnia", subtitle = "W latach 2019-2022",
       x = "Dzień tygodnia", y = "Liczba piosenek") +
  scale_x_discrete(guide = guide_axis(angle = 45))  -> zad2

zad2

# odp. Widzimy, że w każdym roku rozkał liczby wypuszczonych piosenek wsgledem dnia tygodnia wyglada podobnie 
# (choc zmienia sie zdecydowanie skala, widzimy że sumarycznie z roku na rok wypuszczanych piosenek jest coraz wiecej)
# Zdecydowanie najwiecej piosenek wypusczanych jest w piątki, w resztę dni bardzo wartosci są znacząco niższe




# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

spotify %>% 
  mutate(nn = as.numeric(streams)/in_spotify_playlists) %>% 
  slice_max(nn, prop = 0.2) %>% 
  ggplot(aes(x = bpm, fill = mode)) +
  scale_fill_manual(labels = c("Durowa", "Molowa"), values = c("blue","orange"))+
  geom_density(alpha = 0.4) +
  labs(title = "Rozkład tempa piosenek wgledem skali", subtitle = "Dla 20% piosenek najczęściej odtwarzanych
  w przeliczeniu na liczbe playlist spotify",
       x = "Tempo (bpm)", y = "Gęstość",fill = "Skala") -> zad3
zad3  

# odp. Widzimy, że dla obu tonacji (skali) rozkład tempa tych piosenek jest podobny i kumuluje sie w okolicy 120-130 bmp
# dosc duzo jest tez piosenek o wolniejszym tempie (przy okolo 90 nadal jst ich duzo), natomiast piosenek powyzej np 150 bmp
# jest juz bardzo malo



# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

spotify %>% 
  group_by(artist.s._name) %>% 
  summarise(sum_of_streams = sum(as.numeric(streams), na.rm = TRUE)) %>% 
  slice_max(sum_of_streams, n = 10) %>% 
  mutate(artist = fct_reorder(artist.s._name, -sum_of_streams)) %>% 
  ggplot(aes(artist, sum_of_streams)) +
  geom_col(color = "darkblue", fill = "lightblue") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "10 artystów z największą sumą odtworzeń ich piosenek", x = "artysta",
       y = "suma odtworzeń") +
  theme_minimal() -> zad4
zad4

# odp. Widzimy, że 10 artystów z największą liczbą odtworzeń w badanych latach to: The Weeknd, Taylor Swift, Ed Sheeran, 
# Harry Styles, Bad Bunny, Olivia Rodrigo, Eminem, Bruno Mars, Arctic Monkeys i Imagine Dragons (w kolejności do artysty
# posadającego największą liczbę odtworzeń). Bardzo blisko siebie jest czołowo trójka, potem licza odtworzen spada troche szybciej



# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>% 
  mutate(nr_of_artists = as.character(artist_count)) %>% 
  ggplot(aes(x = danceability_., energy_.)) +
  geom_point(color = "darkblue") +
  facet_wrap(~ nr_of_artists) +
  labs(title = "Zależność energetyczności piosenek od ich taneczności", subtitle = "Porównanie dla różnej liczby artystów",
       x = "Taneczność", y = "Energetycznosc") +
  theme_grey()+
  theme(strip.background = element_rect(color = "black", fill = "lightblue")) -> zad5
zad5

# odp. Ciezko dostrzec tu jakąś zależność od liczby artystów (ewentualnie różną liczę danych), natomiast da sie zaobserwowac
# że wraz ze wzrostem taneczności rośnie tez energetyczność piosenek, co w sumie jest dosyć logiczne, gdyz 
# taneczne piosenki w wiekszosci są dość energiczne, choc ocztywiscie zdarzają się wyjątki (co rowniez widac na wykresach)



# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

spotify %>% 
  filter(released_year <=2023, released_year >= 2013) %>% 
  group_by(released_year) %>%
  mutate(year = as.character(released_year)) %>% 
  filter(streams == max(as.numeric(streams))) %>% 
  ggplot(aes(y = streams, x = year)) +
  geom_col(fill = "lightgreen", color = "black") +
  scale_y_discrete(guide = guide_axis(angle = 25)) +
  geom_text(aes(label = track_name, angle = 90), vjust = -0.4, hjust = 1, size = 3)+
  labs(title = "Najpopularniejsze piosenki na przestrzeni lat 2013-2023", x = "Rok",
       y = "Liczba odsluchań") -> zad6
zad6

# Widzimy, że najpopularniejszymi piosenkami w tych latach byly:
# Take Me To The Church (2013), Thinking Out LOud (2014), Love Yourself (2015), One Dance (2016), Shape of You (2017),
# Someone Yo LOved (2018), Blinding Lights (2019), Heat Waves (2020), STAY (2021), As It Was (2022) oraz Flowers (2023).



# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?


spotify %>% 
  filter(released_year == 2022) %>% 
  group_by(released_month) %>% 
  summarise(Spotify = ifelse(in_spotify_playlists > 0, 1, 0),
         Apple = ifelse(in_apple_playlists > 0, 1, 0)) %>% 
  pivot_longer(cols = c(Spotify, Apple)) %>% 
  group_by(released_month, name) %>% 
  summarise(nr = sum(value)) %>% 
  mutate(month = months(as.POSIXlt(paste("2024-", released_month,"-15", sep = "")))) %>% 
  mutate(month = factor(month,c("styczeń", "luty", "marzec", "kwiecień", "maj", "czerwiec", "lipiec", "sierpień", 
                                "wrzesień", "październik","listopad", "grudzień"))) %>% 
  ggplot(aes(month, nr, fill = name)) +
    geom_col(position = position_dodge())+
    theme_grey()+
    labs(title = "Zależność sumy piosenek na playlistach od miesiąca w 2022 roku", subtitle = "Porównanie playlist spotify i apple",
         x = "Miesiąc", y = "Suma", fill = "Platforma") +
    scale_x_discrete(guide = guide_axis(angle = 30))+
  scale_fill_manual(values = c("orange", "darkblue")) -> zad7
zad7

# odp. Widzimy na pewno, że liczba piosenek na obu platformach jest zdecydowanie najwyzsza w maju i rowniez dosc wysoka
# w styczniu, pazdzierniku i grudniu. Widac tez, ze zwykle liczba piosenek na playlistach na spotify jest delikatnie
# wyzsza od liczby piosenek na playlistach apple, jednak nie są to jakieś wielkie roznice



