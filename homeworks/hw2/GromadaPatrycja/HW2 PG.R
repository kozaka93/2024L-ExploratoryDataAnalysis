spotify <- read.csv("spotify-2023.csv")

library(ggplot2)
library(dplyr)
library(forcats)
# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify$streams <- as.numeric(as.character(spotify$streams))



#słupki
spotify %>% filter(released_year == 2023 & released_month <= 3 ) %>%
  ggplot( aes(x = factor(artist_count), y = streams)) +
  geom_col(fill = "hotpink") +
  labs(
    title = "Rozkład liczby odtworzeń piosenek w roku 2023 w 1 kwartale w zależności od liczby wykonawców",
    x = "Liczba Wykonawców",
    y = "Liczba Odtworzeń"
  )
#rozkład
spotify %>% filter(released_year == 2023 & released_month <= 3 ) %>% 
  ggplot( aes(x = factor(artist_count), y = streams)) +
  geom_violin(fill = "hot pink") +
  geom_boxplot(width=0.1) +
  labs(
    title = "Rozkład liczby odtworzeń piosenek w roku 2023 w 1 kwartale w zależności od liczby wykonawców",
    x = "Liczba Wykonawców",
    y = "Liczba Odtworzeń"
  ) 

#max <- spotify %>% filter(released_year == 2023 & released_month <= 3 ) %>% arrange(-streams) %>% head(1)

#Jak widzimy po wykresie słupkowym, najwięcej odtworzeń w wyznaczonym czasie mają utwory z dwoma wykonawcami -
# nieznacznie góruje nad słupiem z jednym wykonawcą.
#Bardzo mało wyświetleń mają utwory z trzema wykonawcami.

#Drugi wykres był zrobiony for fun. Można zauważyć, że jeden outliner znacząco się wyróżnia.
#Jest po utwór "Flowers" Miley Cytrus - obecnie, po roku, na youtube ma 740 milionów odtworzeń.

# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

#Konwertowanie daty na dzień tygodnia
spotify$release_date <- as.Date(paste(spotify$released_year, spotify$released_month, spotify$released_day, sep = "-"))
spotify$day_of_week <- weekdays(spotify$release_date)
spotify$day_of_week <- factor(spotify$day_of_week, levels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela"))


spotify %>% filter(released_year <= 2022 & released_year >= 2019 ) %>% 
ggplot( aes(x = factor(released_year), fill = factor(day_of_week))) +
geom_bar(position = "fill") +
labs(
    title = "Procentowy rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat",
    x = "Rok",
    y = "Procentowa liczba piosenek",
    fill = "Dzień tygodnia"
) +
scale_x_discrete(labels = c("2019", "2020", "2021", "2022")) +
theme_minimal()


#Z wykresu wynika, że najwięcej piosenek wypuszcza się w piątek - w sumie ma sens, aby
#zrobić to przed weekendem. Jest pewna tendencja, że w im bliżej weekendu,
#tym większy procent wypuszczonych piosenek. 


# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?


spotify %>% 
  filter(in_spotify_playlists>=quantile(
    in_spotify_playlists, probs = 4/5)) %>% 
  group_by(mode) %>% 


ggplot(aes(x = factor(mode), y = bpm, fill = mode)) +
  geom_violin() + 
  labs(
    title = "Rozkład tempa względem skali dla piosenek w 20% najczęściej odtwarzanych",
    x = "Skala",
    y = "Tempo",
    fill = "Skala"
  )  + 
  guides(fill = FALSE) +  
  theme_minimal()

#Piosenki ze skali Minor mają większy rozrzut. Najwięcej jest ich w tempie 120 bpm. 
#Widzimy też, że piosenki ze skali Major mają większy zakres bpm.
#Niezależnie od skali, najwięcej piosenek mają tempo pomiedzy 100 a 140bpm.

# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

spotify %>%
  group_by(artist.s._name) %>% 
  mutate(suma = sum(as.numeric(streams), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(artist.s._name = fct_reorder(artist.s._name, suma)) %>%
  distinct(artist.s._name, suma) %>%
  arrange(desc(suma)) %>%
  head(10) -> df
  
df %>% 
ggplot(aes(x = reorder(artist.s._name, suma), y = suma)) +
  geom_bar(stat = "identity", fill = "hot pink") +
  labs(
    title = "Top 10 artystów z największą liczbą odtworzeń wszystkich swoich piosenek",
    x = "Artysta",
    y = "Liczba odtworzeń"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() +
  theme(axis.text.y = element_text(hjust = 0.5))
  
#Najwięcej odtworzeń ma zespół "The Weeknd". Nieznacznie za nimi jest Taylor Swift i Ed Sheeran.
#Najmniej w tym zestawieniu ma zespół "Imagine Dragons"



# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

ggplot(spotify, aes(x = danceability_., y = energy_., color = factor(artist_count))) +
  geom_point(size = 2) + 
  labs(
    title = "Zależność energetyczności od taneczności w zależności od liczby artystów",
    x = "Taneczność",
    y = "Energetyczność",
    color = "Liczba artystów"
  )  +
  theme_minimal() +
  scale_color_manual(values = c("black", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#17becf"))

#cóż mogę powiedzieć. Widzimy, że najwięcej piosenek mają pojedynczy wykonawcy,
#zdecydowanie dominują czarne kropki. Mają one również największy rozrzut.
#Możemy zauważyć tendencję, że wraz ze wzrostem czy energatycznosci czy tanecznosci wzrasta
#to drugie, Najwięcej kropek zbiera się w kwadracie 50 - 80 w osi X i 50 - 80 w osi Y.


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

spotify %>%
  filter(released_year <= 2023 & released_year >= 2013) %>% 
  group_by(released_year) %>%
  slice(which.max(streams)) %>%
  ungroup() %>% 
  select(released_year, streams, track_name) %>% 
  arrange(released_year) -> df2

df2 %>% 
ggplot(aes(x = factor(released_year), y = as.numeric(streams), fill = track_name)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = track_name), position = position_stack(vjust = 0.5), angle = 90, size = 3) +
  labs(title = "Najpopularniejsza piosenka w każdym roku 2013-2023",
       x = "Rok",
       y = "Liczba odtworzeń",
       fill = "Piosenka") +
  theme_minimal() +
  guides(fill = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#print(df2)
#Załączam tabelke:
# released_year    streams track_name               
# 1          2013 2135158446 Take Me To Church        
# 2          2014 2280566092 Thinking Out Loud        
# 3          2015 2123309722 Love Yourself            
# 4          2016 2713922350 One Dance                
# 5          2017 3562543890 Shape of You             
# 6          2018 2887241814 Someone You Loved        
# 7          2019 3703895074 Blinding Lights          
# 8          2020 2557975762 Heat Waves               
# 9          2021 2665343922 STAY (with Justin Bieber)
# 10          2022 2513188493 As It Was                
# 11          2023 1316855716 Flowers  

#w latach 2013-2023 najpopularniejsza piosenka byla Blinding Lights (wypuszczona w 2019r).


# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

df3 <- spotify %>%
  filter(released_year == 2022) %>%
  group_by(released_month) %>%
  summarize(total_songs_spotify = sum(in_spotify_playlists),
            total_songs_apple = sum(in_apple_playlists))

#wykres liniowy
ggplot(df3, aes(x = released_month)) +
  geom_line(aes(y = total_songs_spotify, color = "Spotify"), size = 2) +
  geom_line(aes(y = total_songs_apple, color = "Apple Music"), size = 2) +
  labs(title = "Zależność liczby piosenek na playlistach w 2022 roku",
       x = "Miesiąc",
       y = "Suma piosenek na playlistach",
       color = "Platforma muzyczna") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_y_log10()+
  scale_color_manual(values = c("Spotify" = "green", "Apple Music" = "blue")) +
  theme_minimal()


# # Wykres kropkowy
# ggplot(df3, aes(x = total_songs_spotify, y = total_songs_apple, color = factor(released_month))) +
#   geom_point(size = 3) +
#   labs(title = "Zależność liczby piosenek na playlistach Spotify i Apple w 2022 r",
#        x = "Liczba piosenek na playlistach Spotify",
#        y = "Liczba piosenek na playlistach Apple Music",
#        color = "Miesiąc") +
#   theme_minimal()

#zdecydowanie wiecej jest piosenek w playlistach Spotify w każdym z miesięcy.
#Ale wykresy wyglądają bardzo podobnie, krztalt wykresu zostal zachowany w obu przypadkach,
#pomimo znacząco różniej ilości piosenek. Bez skali log nie widać tej tendencji. 

