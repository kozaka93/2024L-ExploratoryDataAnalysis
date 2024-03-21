library(dplyr)
library(ggplot2)
library(tidyr)

spotify <- read.csv("spotify-2023.csv")

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?
z1 <- spotify %>% 
  filter(released_year == 2023, released_month <= 3) %>% 
  mutate(odtworzen = as.double(streams)/1e+06,
         wykonawcy = as.character(artist_count)) 
  
ggplot(z1, aes(x = wykonawcy, y = odtworzen)) + 
  geom_violin()+
  labs(title = "Liczba odtworzeń piosenek 
w pierwszym kwartale 2023 roku",
       x = "Liczba wykonawców",
       y = "Liczba odtworzeń piosenek 
(w milionach)")+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()

#Największy rozstęp watości jest dla piosenek z jednym wykonawcą,
#dla piosenek z dwoma wykonawcami rozkład odtworzeń jest najbardziej 
#zbliżony do jednostajnego, natomiast dla piosenek z trzema wykonawcami
#rozstęp jest najmniejszy i watości są najbardziej zbliżone do jednej watości
#najpopularniejsza piosenka wykonywana jest przez jedego wykonawcę, jest to
#piosenka znacznie odbiegająca liczbą odtworzeń od pozostałych.


# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?
z2 <- spotify %>% 
  filter(released_year <= 2022, released_year >= 2019) %>%
  mutate(dzientygodnia = weekdays(ISOdate(released_year, released_month, released_day))) %>%
  mutate(rok = as.character(released_year)) %>% 
  group_by(dzientygodnia, rok) %>% 
  summarise(n = n())

ggplot(z2, aes(x = dzientygodnia, y = n, fill = rok)) +
  geom_col()+
  labs(title = "Zależność liczby wypuszczonych 
piosenek od dnia tygodnia",
       subtitle = "W latach 2019-2022",
       x = "dzień tygodnia",
       y = "liczba piosenek",
       fill = "Rok")+
  scale_fill_manual(values = c("#e41a1c","#377eb8","#4daf4a","#984ea3"))+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  theme_minimal()

#Najwięcej piosenek było wypuszczanych w piątki w każdym z rozpatrywanych
#lat (jest to kilkakrotnie więceje niż w pozostałe dni), drugi najpopulaniejszy
#dzień to czwartek, pozostałe dni uzyskują znacznie mniejsze wartości
#widać też że najwięcej piosenek było wypuszczanych w 2022 roku w porównaniu
#z pozostałymi latami

# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?
z3 <- spotify %>%
  top_frac(0.2, in_spotify_playlists) %>% 
  group_by(mode, bpm) %>% 
  summarise(n = n())

ggplot(z3, aes(x = mode,  y = bpm, fill = mode)) +
  geom_violin()+
  labs(title = "Tempo względem skali",
       subtitle = "dla najczęściej odtawarzanych piosenek",
       x = "skala",
       y = "tempo",
       fill = "Rok")+
  scale_fill_manual(values = c("#e41a1c","#4daf4a"))+
  theme(legend.position = "none")

#Dla skali Minor wartości tempa układają się bliżej jednej najczęściej występującej
#wartości, dla skali Major są one rozłożone bardziej równomiernie. 
#Tempo dla skali Major ma większy rozstęp wartości


# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

z4 <- spotify %>% 
  filter(artist_count == 1) %>%
  group_by(artist.s._name) %>%
  summarise(n = sum(as.double(streams)/1e+09)) %>% 
  top_n(10, n) %>% 
  mutate(artist.s._name = forcats::fct_reorder(artist.s._name, n))

ggplot(z4, aes(x = artist.s._name, y = n))+
  geom_col()+
  labs(title = "Artyści z największą liczbą odtworzeń piosenek",
       y = "liczba odtowrzeń 
(w miliardach)",
       fill = "Rok")+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  theme(axis.title.x = element_blank())

#Artyści według rosnącej liczby odworzeń w top10 to Imagine Dragons, Artic Monkeys,
#Bruno Mars, Eminem, Olivia Rodrigo, Bad Bunny, Harry Styles, Ed Sheeran, Taylor Swift,
#The Weekend. Różnice między kolejnym miejscami są największe w okolicy 4-5 miejsca
#w top 10. Najchętniej słuchana trójka artystów ma bardzo zbiliżone liczby odtworzeń
#swoich piosenek

# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

z5 <- spotify %>%
  mutate(wykonawcow = as.character(artist_count))

ggplot(z5, aes(y = energy_., x = danceability_.))+
  geom_point()+
  geom_smooth()+
  facet_grid(~wykonawcow)+
  labs(title = "Zależność energetyczności od taneczności",
       x = "energetyczność (częstość na minutę)",
       y = "taneczność")+
  theme_bw()

#Najwięcej analizowanych piosenek ma jednego wykonawcę, dane te są bardzo roproszone
#i trudno wskazać jedoznaczną zależność między tanecznością a tanecznością a energetycznością,
#widać jednak wzost średniej taneczności w zależności od eneregetyczności dla energetyczności
#mniejszej niż 60. Dla dwóch wykonawców średnia taneczność układa się praktycznie na stałym 
#poziomie względem eneregtyczność. Dane dla trzech wykonawców w większości układaja się w 
#przedziale energetyczności 60-80 dla wszystkich wartości taneczności. Dla pozostałych 
#wartości liczby artystów, piosenek jest mało i nie można wyciągnąć wniosków co do zależności
#eneregtyczności od taneczności.


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

z6 <- spotify %>% 
  filter(released_year >= 2013, released_year <= 2023) %>% 
  group_by(released_year) %>% 
  top_n(1, as.double(streams)) %>% 
  mutate(year = as.character(released_year),
         track_name = forcats::fct_reorder(track_name, year),
         odtworzen = as.double(streams)/1e+06)


ggplot(z6, aes(x = year, y = odtworzen, fill = track_name))+
  geom_col()+
  labs(title = "Najbardziej popularne piosenki",
       subtitle = "W latach 2013-2023",
       x = "rok",
       y = "liczba odtworzeń
(w milionach)",
       fill = "piosenka")+
  scale_fill_brewer(palette = "Set3")+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  theme_grey()

#Najpopularniejsze piosenki w kolejnych latach to Take Me To Church: 
#Take Me To Church(2013), Thinking Out Loud(2014), Love Yourself(2015),
#One Dance(2016), Shape of You(2017), Someone You Loved(2018), 
#Blinding Lights(2019), Heat Waves(2020), STAY (with Justin Bieber)(2021),
#As It Was(2022), Flowers(2023). Największą liczbę odtworzeń z tych piosenek
#uzyskała Blinding Lights w 2019 roku.


# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

z7 <- spotify %>% 
  filter(released_year == 2022) %>% 
  mutate(month = forcats::fct_reorder(format(ISOdate(released_year, released_month, released_day), "%B"), released_month))%>% 
  group_by(month) %>%
  summarise(spot = sum(in_spotify_playlists)/1000,
            apple = sum(in_apple_playlists)/1000) %>% 
  pivot_longer(cols = c(spot, apple))  


ggplot(z7, aes(x = month, y = value, color = name))+
  geom_point()+
  labs(title = "Liczba piosenek na playlistach",
       subtitle = "W 2022 roku",
       x = "miesiąc",
       y = "suma liczby piosenek
(w tysiącach)")+
  scale_color_manual(values=c("#E69F00", "#56B4E9"), 
                    name="Platforma",
                    labels=c("Apple", "Spotify"))+
  scale_x_discrete(guide = guide_axis(angle = 90))

#Liczba piosenek na playlistach na Spotify jest znacznie większa niż na 
#playlistach na Apple w każdym miesiącu. Najwięcej piosenek na obu platformach
#było w maju, wtedy też była największa różnica między Apple a Spotify

