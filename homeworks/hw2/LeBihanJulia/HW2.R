spotify <- read.csv("spotify-2023.csv")
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?


dane1 <- spotify %>% 
   filter(released_year == 2023 & (released_month == "1" | released_month == "2" | released_month == "3"))  


 w <- dane1 %>%
  ggplot( aes(x=factor(artist_count), y=as.numeric(streams), fill=factor(artist_count))) +
  geom_boxplot() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=10)
  ) +
  ggtitle("Liczba odtworzeń w zależności od liczby wykonawców (I kwartał 2023)") +
  xlab("Liczba artystów") +
  ylab("Liczba odtworzeń")
 
ggplotly(w)


# Mediana liczby odtworzeń jest największa dla dwóch artystów a najmniejsza dla trzech.
# Największa rozpiętość między pierwszym a trzecim kwartylem również jest dla dwóch artystów.
# Dla jednego i dwóch artystów pojawiają się obserwacje znacznie odstające od innych 
# (dla jednego artysty - 4 a dla dwóch - 1). 
# Największą liczbę odsłon miała piosenka jednego artysty (ale jest to outlier).


# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

dane2 <- spotify 

dane2$data<-as.Date(with(dane2,paste(released_year,released_month, released_day,sep="-")),"%Y-%m-%d")

dane2 <- dane2 %>% 
  filter(released_year >= 2019 & released_year <= 2022) %>% 
  mutate(dzien_tygodnia = wday(data, label = TRUE, abbr = FALSE)) %>% 
  group_by(released_year, dzien_tygodnia) %>% 
  summarize(ile_wypuszczonych = length(track_name))

ggplot(dane2, aes(x = factor(dzien_tygodnia), y = as.numeric(ile_wypuszczonych))) +
  geom_bar(stat = "identity", color = "black", fill=rgb(0.3, 0.7, 0.9)) +
  theme_bw() +
  ggtitle("Liczba piosenek względem dni tygodnia (2019-2022)") +
  theme(plot.title = element_text(size = 12), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Dzień tygodnia") +
  ylab("Liczba piosenek") +
  facet_wrap(~released_year)

# W każdym roku najwięcej wypuszczonych piosenek było w piątek, przy czym w ostatnim roku (2022)
# łączna liczba piosenek wzrosła, a największy przyrost można zaobserwować właśnie w piątek.



# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?



dane3 <- spotify %>% 
  top_n(floor(953*0.2), as.numeric(streams)/in_spotify_playlists) %>% 
  select(track_name, mode, bpm) %>% 
  rename("skala" = "mode")


ggplot(dane3, aes(x = as.numeric(bpm), fill = skala)) +
  geom_density(alpha = 0.45) +
  theme_bw() +
  ggtitle("Rozkład tempa względem skali (top 20% pod względem odtworzeń na liczbę playlist spotify)") +
  theme(plot.title = element_text(size = 9)) +
  xlab("Tempo (bpm)") +
  ylab("Rozkład")

# Rozkłady te są dość zbliżone.
# Najwięcej piosenek w obu przypadkach ma tempo z przedziału 100-150.
# Liczba utworów z tempem powyżej 150 spada, zwłaszcza dla piosenek ze skali "Minor".
# Utworów z tempem przekraczającym 150 jest nieco więcej o skali "Major".


# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

dane4 <- spotify %>% 
  group_by(artist.s._name) %>% 
  summarise(laczna_liczba_odtworzen = sum(as.numeric(streams))) %>% 
  arrange(desc(laczna_liczba_odtworzen)) %>% 
  head(10)

ggplot(dane4, aes(x = artist.s._name, y = laczna_liczba_odtworzen)) +
  geom_bar(stat = "identity", color = "black", fill=rgb(0.3, 0.7, 0.9)) +
  theme_bw() +
  ggtitle("Top 10 artystów o największej liczbie odtworzeń wszystkich piosenek") +
  theme(plot.title = element_text(size = 11), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Artysta") +
  ylab("Liczba odtworzeń wszystkich piosenek") 

# Artyści o największej liczbie odtworzeń wszystkich piosenek w sumie (top 10):
#   artysta          laczna_liczba_odtworzen
#                             
# 1 The Weeknd                   14185552870
# 2 Taylor Swift                 14053658300
# 3 Ed Sheeran                   13908947204
# 4 Harry Styles                 11608645649
# 5 Bad Bunny                     9997799607
# 6 Olivia Rodrigo                7442148916
# 7 Eminem                        6183805596
# 8 Bruno Mars                    5846920599
# 9 Arctic Monkeys                5569806731
#10 Imagine Dragons               5272484650

round(14185552870/5272484650, 1)

# Widać różnicę pomiędzy łączną liczbą odtworzeń wszystkich piosenek artysty, który jest na pierwszym
# miejscu a łączną liczbą odtworzeń wszystkich piosenek artysty, który jest na miejscu 10.
# (Piosenki The Weeknd miały 2,7 razy więcej odtworzeń niż piosenki Imagine Dragons).

# Istnieje niewielkie zróżnicowanie między trzema pierwszymi artystami. Relatywnie niewielkie zróżnicowanie 
# jest również między trzema ostatnimi artystami.


# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>% 
  group_by(artist_count) %>% 
  summarise(ile = length(track_name)) %>% 
  View()

# Liczba artystów waha się od 1 do 8, przy czym przeważają piosenki wykonywane przez 1,2 lub 3 artystów.
# Pozostałe pojawiają się rzadko.

p <- ggplot(spotify, aes(x=`danceability_.`, y=`energy_.`, color=factor(artist_count))) + 
  geom_point() +
  theme_bw() +
  ggtitle("Energetyczność a taneczność przez pryzmat liczby artystów") +
  theme(plot.title = element_text(size = 11)) +
  xlab("Taneczność") +
  ylab("Energetyczność") +
  labs(color = "liczba artystów")


ggplotly(p)

# Jeden wykres nie dawał właściwej czytelności, dlatego rozdzieliłam na dwa.
# (pierwszy - dane dla piosenek wykonywanych przez 1-3 artystów i drugi dla pozostałych piosenek)

dane5_1 <- spotify %>% 
  filter(artist_count >=1 & artist_count <=3)

p1 <- ggplot(dane5_1, aes(x=`danceability_.`, y=`energy_.`, color=factor(artist_count))) + 
  geom_point() +
  theme_bw() +
  ggtitle("Energetyczność a taneczność przez pryzmat liczby artystów (1-3)") +
  theme(plot.title = element_text(size = 10.5)) +
  xlab("Taneczność") +
  ylab("Energetyczność") +
  labs(color = "liczba artystów")


ggplotly(p1)

dane5_2 <- spotify %>% 
  filter(artist_count >=4)

p2 <- ggplot(dane5_2, aes(x=`danceability_.`, y=`energy_.`, color=factor(artist_count))) + 
  geom_point() +
  theme_bw() +
  ggtitle("Energetyczność a taneczność przez pryzmat liczby artystów (4-8)") +
  theme(plot.title = element_text(size = 10.5)) +
  xlab("Taneczność") +
  ylab("Energetyczność") +
  labs(color = "liczba artystów")

ggplotly(p2)

# Energetyczność i taneczność piosenek wykonywanych przez jednego artystę (tych utworów jest najwięcej) 
# rozkłada się dość równomiernie. (taneczność od 35 do 90 i energetyczność od 25 do 90).
# Natomiast piosenki wykonywane przez dwóch lub trzech artystów zwykle mają większą taneczność i energetyczność
# (taneczność - z przedziału 60-90, energetyczność 50-90)
# Piosenek wykonywanych przez 4 do 8 artystów jest bardzo mało. Można jedynie zauważyć, że ich taneczność
# (poza jedną) zaczyna się od około 55 a energetyczność od około 45.



# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?


dane6 <- spotify %>% 
  filter(released_year >= 2013 & released_year <= 2023) %>% 
  group_by(released_year) %>% 
  slice(which.max(as.numeric(streams))) %>% 
  select(track_name, released_year, streams) 


ggplot(dane6, aes(x = factor(released_year), y = as.numeric(streams))) +
  geom_bar(stat = "identity", color = "black", fill=rgb(0.3, 0.7, 0.9, 0.5)) +
  theme_bw() +
  ggtitle("Liczba odtworzeń najbardziej popularnych piosenek (2013-2023)") +
  theme(plot.title = element_text(size = 12)) +
  xlab("Rok powstania piosenki") +
  ylab("Liczba odtworzeń") +
  geom_text(label=dane6$track_name, hjust = 1.2, size = 3) +
  coord_flip()

# Najbardziej popularnymi piosenkami w latach 2013-2023 były:
#   track_name                 released_year    streams   
#  1 Take Me To Church                  2013 2135158446
#  2 Thinking Out Loud                  2014 2280566092
#  3 Love Yourself                      2015 2123309722
#  4 One Dance                          2016 2713922350
#  5 Shape of You                       2017 3562543890
#  6 Someone You Loved                  2018 2887241814
#  7 Blinding Lights                    2019 3703895074
#  8 Heat Waves                         2020 2557975762
#  9 STAY (with Justin Bieber)          2021 2665343922
# 10 As It Was                          2022 2513188493
# 11 Flowers                            2023 1316855716


# Piosenka "Blinding Lights" z 2019 roku miała rekordową liczbę odtworzeń. Piosenka "Flowers" z 2023 miała
# najmniej odtworzeń (w porównaniu z liczbą odtworzeń najbardziej popularnych piosenek z lat 2013-2023.)



# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

dane7 <- spotify %>% 
  filter(released_year == 2022) %>% 
  group_by(released_month) %>% 
  mutate(ile_na_spotify = sum(in_spotify_playlists)) %>% 
  mutate(ile_na_apple = sum(in_apple_playlists)) %>% 
  select(released_month, ile_na_spotify, ile_na_apple) %>% 
  distinct() %>% 
  arrange(released_month)


# dane7 jako inaczej wyglądająca ramka danych 

k1 <- rep(dane7$released_month, each = 2)
apple_czy_spotify <- rep(c("spotify", "apple"), 12)
k3 <- c(rbind(dane7$ile_na_spotify, dane7$ile_na_apple))
data2 <- data.frame(k1, apple_czy_spotify, k3)

# Porównanie 

ggplot(data2, aes(fill = apple_czy_spotify, y = k3, x = factor(k1))) +
  geom_bar(position="dodge", stat="identity") +
  theme_bw() +
  theme(
    
    plot.title = element_text(size=10)
  ) +
  ggtitle("Suma piosenek na playlistach spotify i apple po miesiącach (2022)") +
  xlab("Miesiące") +
  ylab("Liczba playlist zawierających daną piosenkę")+
  scale_fill_discrete(name = "apple czy spotify")

# Dwa oddzielne wykresy dla spotify i dla apple


ggplot(dane7, aes(x = factor(released_month), y = as.numeric(ile_na_spotify))) +
  geom_bar(stat = "identity", color = "black", fill=rgb(0.3, 0.7, 0.9)) +
  theme_bw() +
  ggtitle("Suma piosenek na playlistach spotify po miesiącach (2022)") +
  theme(plot.title = element_text(size = 12)) +
  xlab("Miesiące") +
  ylab("Suma piosenek")

ggplot(dane7, aes(x = factor(released_month), y = as.numeric(ile_na_apple))) +
  geom_bar(stat = "identity", color = "black", fill=rgb(0.3, 0.7, 0.9)) +
  theme_bw() +
  ggtitle("Suma piosenek na playlistach apple po miesiącach (2022)") +
  theme(plot.title = element_text(size = 12)) +
  xlab("Miesiące") +
  ylab("Suma piosenek")

# Piosenek na playlistach apple'a jest zdecydowanie mniej niż na plylistach spotify'a.
# Natomiast w obu przypadkach widać, że miesiącem, w którym suma piosenek jest największa
# jest maj a najmniejsza - wrzesień, przy czym dla spotify wartość majowa znacznie odbiega od pozostałych.




