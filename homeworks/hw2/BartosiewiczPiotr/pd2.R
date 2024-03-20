library(dplyr)
library(ggplot2)

spotify <- read.csv("spotify-2023.csv")

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>%
  filter(released_year == 2023 & released_month <= 3) %>%
  ggplot(aes(as.factor(artist_count), y = as.numeric(streams))) +
  geom_violin() +
  theme_minimal() + 
  labs(x="liczba wykonawców", 
        y = "liczba odtworzeń piosenek",
       title = "Rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
       w zależności od liczby wykonawców")

spotify %>%
  filter(released_year == 2023 & released_month <= 3) %>%
  ggplot(aes(as.factor(artist_count), y = as.numeric(streams))) +
  geom_boxplot() +
  theme_minimal() + 
  labs(x="liczba wykonawców", 
       y = "liczba odtworzeń piosenek",
       title = "Rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
       w zależności od liczby wykonawców")

  
# z wykresy widac, że najwiecej najbardziej popularnych piosenek posiadają jednego wykonawace,
# kolejna rzecza jaka mozna zauwazazyc jest to, ze wszytskie maja "najgęstrszy" rozklad 
# w tym samym miejscu na osi OY


# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?


spotify %>%
  mutate(dzien_tygodnia = weekdays(as.Date(paste(paste(released_year,released_month	,released_day, sep="-"))))) %>%
  filter(2019<=released_year & released_year <=2022) %>%
  group_by(released_year, dzien_tygodnia) %>%
  summarise(liczba_wypuszczonych_piosenek = n()) %>%
  ggplot(aes(x =as.factor(dzien_tygodnia), y = liczba_wypuszczonych_piosenek)) +
  geom_col(aes(fill=as.factor(released_year))) +
  theme_minimal() + 
  labs(
    x = 'Dni tygodnia',
    y = 'Riczba wypuszczonych piosenek',
    title = 'Rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między 2019 a 2022?',
    fill = "Rok:"
    )

# Widzimy, że bez znaczenia od roku dniem, w którym najczęściej wypuszcza się
# piosenki jest piątek, kolejną rzeczą którą można zauważyć jest znacząca przewaga 
# piosenek wypuszczonych w 2022 w porownaniu z innymi latami


# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?


spotify %>% 
  arrange(desc(in_spotify_playlists)) %>% 
  head(ceiling(nrow(spotify)*0.2)) %>%
  ggplot(aes(x = mode, y = bpm)) +   
  geom_violin()  +   
  labs(x = "skala", y = "tempo bpm", title = "Rozkład tempa względem skali dla piosenek, które są w 20% 
 najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify") +   
  theme_minimal()


# wykres dla major jest bardziej splaszczony, a wykres skupia wiekszosc obreswacji
# w okolicy 120 tempa bmp
  




# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?



spotify %>%
  group_by(artist.s._name) %>%
  summarise(liczba_piosenek = sum(as.numeric(streams), na.rm = TRUE)) %>%
  arrange(desc(liczba_piosenek)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(artist.s._name, desc(liczba_piosenek)), y =liczba_piosenek )) +
  geom_col(position = "stack", aes(fill = artist.s._name)) + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(
    x='',
    y="liczba piosenek",
    title = "Pierwsza dziesiątka artystów posiadających najwięcej odtworzeń wszystkich swoich piosenek w sumie"
  ) +
  theme(axis.text.x = element_text(angle=45, hjust=1))
  

# z wykresu dostajemy informacje, ze najbardziej popularnymi wykonawcami sa
# The Weekend, Taylor Swift i Ed Sheeran, reszta artystow ma juz zauwazalnie 
# mniej odtworzen


# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?


spotify %>%
  ggplot(aes(x=danceability_., y=energy_., color=artist_count)) +
  geom_point() +
  labs(x = "Taneczność", 
       y = "Energia", 
       color = "Skala",
       title = 'Zależność taneczności od energii "patrząc przez pryzmat" liczby artystów.') +
  theme_bw() +
  theme(legend.position = "top") +
  scale_x_continuous(limits = c(0,100)) + 
  scale_y_continuous(limits = c(0,100))  +
  scale_color_gradient2(
    mid = "orange", high = "red", low = "green",
    limits= c(1,8), midpoint = 4
  )

# trudno powiedziec aby byla jakas roznica w rozstwaieniu w zaleznosci od 
# liczby artysty, ale mozna zauwazac ze jest scisle powiazanie energetycznosci
# utworu z jego tanecznoscia




# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?


spotify %>%
  filter(released_year >= 2013 & released_year <= 2023)%>%
  group_by(released_year) %>%
  top_n(1, streams)  %>%
  mutate(wysokosc=(2023-released_year)/10) %>%
  ggplot(aes(x = as.factor(released_year), y = wysokosc, label = track_name)) +
  geom_text(size = 3, aes(colour= released_year)) +
  labs(title = "Najpopularniejsze piosenki w latach 2013-2023",
       x = "",
       y = "") +
   theme_minimal() +
  scale_y_continuous(breaks = NULL) +
  theme(legend.position = "none")

# na wykresie widac ktora piosenka w danym roku byla
# najbardziej popularna



# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?


spotify %>%
  filter(released_year==2022)%>%
  group_by(released_month) %>%
  summarise(suma_spoti = sum(in_spotify_playlists), suma_apple = sum(in_apple_playlists)) %>%
  ggplot(aes(x= suma_spoti, y =suma_apple, colour =released_month)) +
  geom_point(size=3) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(
    x='Suma piosenek na playlistach na spotify ',
    y = 'Suma piosenek na playlistach na apple',
    title = 'Zależność pomiędzy sumą piosenek na playlistach na spotify, a
    apple w poszczególnych miesiącach 2022 roku',
    colour = "Miesiące"
  ) +
  scale_color_gradient2(
    mid = "orange", high = "red", low = "green",
    limits= c(1,12), midpoint = 6
  )

# zauwazamy prawie liniowa zaleznosc oraz jeden miesiąc
# w ktorym suma piosenek na playlistahc spotify znacznie odstaje
# od reszty.
  





