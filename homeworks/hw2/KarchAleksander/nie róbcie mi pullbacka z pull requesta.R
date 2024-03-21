spotify <- read.csv("spotify-2023.csv")
library(dplyr)
library(ggplot2)
library(forcats)
library(gt)
library(tidyr)
options(scipen = 12, warn = -1)
# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>% 
  filter(released_year == 2023 & released_month %in% c(1,2,3)) %>% 
  ggplot(mapping = aes(x = as.character(artist_count), y = as.integer(streams))) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Rozkład liczby odtworzeń piosenek w zależności od liczby wykonawców",
       subtitle = "Opublikowanych w roku 2023 w pierwszym kwartale",
       x = "Liczba wykonawców",
       y = "Liczba odtworzeń na spotify") +
  theme_bw()
  
# Widzimy, że największa różnorodność występuje wśród piosenek z 2 wykonawcami, z kolei piosenki pisane 
# przez pojedyńczych artystów mają najwięcej outlierów - piosenek bardzo popularnych.
# W grupach 3 osobowych rozrzut jest stosunkowo mały.


# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

spotify %>% 
  filter(released_year %in% c(2019,2020,2021,2022)) %>% 
  mutate(weekday = factor(weekdays(as.Date(paste0(released_year,"-",released_month,"-",released_day))), 
                          levels = weekdays(seq(as.Date("2024-03-18"),as.Date("2024-03-24"), by = 1)))) %>% 
  group_by(released_year, weekday) %>% 
  summarise(n = n()) %>% 
  ggplot(mapping = aes(x = weekday, y = n)) +
  geom_col(fill = "lightblue") +
  labs(title = "Rozkład liczby wypuszczonych piosenek względem dnia tygodnia",
       subtitle = "Dla poszczególnych lat między 2019 a 2022",
       x = "Dzień tygodnia",
       y = "Liczba wypuszczonych utworów") +
  scale_fill_discrete(name = "Rok wydania")+
  scale_y_continuous(expand = c(0,0,0,1))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme_bw() + 
  facet_wrap(~as.character(released_year), scales = "free_y")

# Możemy zauważyć, że w każdym roku znaczna część piosenek została wydana w piątek.
# Dodatkowo, w roku 2019 nie wydano ani jednej piosenki w poniedziałek i sobotę, a
# w roku 2020 - we wtorek. Ponadto możemy zobaczyć, że liczba wypuszczonych piosenek
# była znacznie większa w roku 2023 niż w pozostałych latach.




# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

spotify %>% 
  mutate(coef_popularity = as.integer(streams)/in_spotify_playlists) %>% 
  filter(coef_popularity >= quantile(coef_popularity, 0.8, na.rm = TRUE)) %>% 
  ggplot(aes(x = mode, y = bpm)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Rozkład tempa względem skali",
       subtitle = "Dla piosenek, które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify",
       x = "Skala",
       y = "Tempo (w bpm)")+
  theme_bw()

# Skala nie ma dużego wpływu na tempo popularnych piosenek. Możemy zauważyć mniejszy
# rozstęp kwartylny w przypadku utworów w skali molowej niż u tych w skali durowej.
# Mediany są do siebie bardzo zbliżone, w okolicach 125 bpm


# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

spotify %>% 
  group_by(artist.s._name) %>% 
  summarise(streams_total = sum(as.integer(streams), na.rm = TRUE)) %>% 
  arrange(-streams_total) %>% 
  head(10) %>% 
  mutate(Artist = fct_reorder(artist.s._name, streams_total)) %>% 
  ggplot(aes(y = Artist, x = streams_total))+
  geom_col(fill = "darkred")+
  labs(title = "Popularność artystów",
     subtitle = "W odtworzeniach piosenek na spotify",
     x = "Liczba odtworzeń",
     y = "Artysta")+
  scale_x_continuous(expand = c(0,0,0.02,0))+
  theme_bw()


# Taylor Swift ma najwięcej odtworzeń swoich piosenek i zostawia konkurencję daleko w tyle
# Drugie i trzecie miejsca - odpowiednio The Weeknd i Bad Bunny mają w okolicach 10 miliardów
# odsłuchań. Na dalszych miejscach różnice są już trochę mniejsze, top10 zamyka SZA
# z wynikiem nieco ponad 4.5 mld odtworzeń.


# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?


spotify %>% 
  ggplot(aes(y = danceability_., x = energy_., color = as.character(artist_count)))+
  geom_point() +
  labs(title = "Rozkład łączny energetyczności i taneczności piosenek",
       subtitle = "z wyszczególnieniem liczby autorów",
       x = "Taneczność",
       y = "Energia")+
  scale_color_discrete(name="Liczba artystów")+
  theme_bw()

# Możemy zauważyć, że piosenki były na ogół taneczne i energetyczne.
# W naszych danych utwory o dużej ilości autorów należą do tych bardziej tanecznych i energetycznych zarazem.
# W grupach 6,7,8 osobowych mamy bardzo mało utworów, przez co trudno mówić o jakiejkolwiek tendencji


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

spt <- spotify %>%
  filter(released_year %in% 2013:2023) %>% 
  group_by(released_year) %>% 
  summarise(str_max = max(as.integer(streams), na.rm = TRUE)) %>% 
  mutate(song = spotify$track_name[which(spotify$streams %in% as.character(str_max))])
colnames(spt)  = c("Rok", "Liczba Odtworzeń", "Nazwa Utworu")
  gt(spt) %>% 
  tab_header(title = md("Najpopularniejsza piosenka w latach 2013-2023"))


# Spodziewam się stracić za ten podpunkt 0.75 punkta, ale zupełnie nie miałem pomysłu
# w jaki sposób zrealizować wykresem po 1 najpopularniejszej piosence z każdego roku
# Zrobiłem tabelę, jaka piosenka była najpopularniejsza każdy widzi, wykres byłby
# przerostem formy nad treścią

# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?
  
spotify %>% 
    filter(released_year == 2022) %>% 
    group_by(released_month) %>% 
    summarise(sum_apple = sum(in_apple_playlists, na.rm = TRUE), sum_spotify = sum(in_spotify_playlists, na.rm = TRUE)) %>% 
    mutate(released_month = factor(month.name, levels = month.name)) %>% 
    pivot_longer(cols = c(sum_apple, sum_spotify)) %>% 
    ggplot(aes(y = released_month, x = value, group = name, fill = name))+
    geom_col(position = "dodge")+
    labs(title = "Zależność pomiędzy sumą piosenek na playlistach spotify i apple",
         subtitle = "Dane miesięczne",
         x = "Liczba piosenek na playlistach (log)",
         y = "Miesiąc")+
    scale_y_discrete(limits=rev)+
    scale_x_continuous(transform = "log10", expand = c(0,0,0,0.1))+
    scale_fill_discrete(name="Serwis muzyczny", labels = c("Apple", "Spotify"))+
    theme_bw()

# Najwięcej piosenek na obu serwisach było w maju, a najmniej we wrześniu.
# Co ciekawe różnice między poszczególnymi miesiącami były znaczne.
# Możemy zobaczyć również jak bardzo spotify deklasuje apple music w tej dziedzinie
# Skala logarytmiczna została zastosowana dla czytelności, natomiast w liczbach absolutnych
# Apple przy spotify ma marginalną ilczbę piosenek na playlistach

    
    
    


