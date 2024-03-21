spotify <- read.csv("spotify-2023.csv")

spotify$streams <- as.numeric(spotify$streams)

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(hrbrthemes)
library(geomtextpath)
library(viridis)
library(lubridate)
library(scales)


View(spotify)
# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>% 
  filter(released_year == 2023, released_month %in% c(1, 2, 3)) %>% 
  mutate(artist_count = factor(artist_count)) %>%
  ggplot(aes(x = artist_count, y = streams))+
    geom_boxplot(color = "black", fill = "lightblue")+
    theme_classic()+
    labs(title = "Rozkład liczby odtworzeń piosenek w zależności od liczby wykonawców",
         subtitle = "Dane na pierwszy kwartał 2023 roku",
         x = "Liczba wykonawców",
         y = "Liczba odtworzeń")
  



#KRÓTKI KOMENTARZ DO ZADANIA:

#Możemy zauważyć, że piosenki, które mają dwóch wykonawców mają największy rozstęp 
#międzykwartylowy, natomiast 3 kwartyl jest znacznie wyższy, niż dla 1 i 3 wykonawców, co
#może nasunąć wniosek, że piosenki z 2 autorami są najczęściej odtwarzane.



# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

moje_kolory <- c("#fdbe85", "#fd8d3c", "#e6550d", "#a63603")

spotify %>% 
  filter(released_year >= 2019 & released_year <= 2022) %>% 
  mutate(released_day_of_week = wday(paste(released_year, released_month, released_day, sep = "-"))) %>% 
  ggplot(aes(x = factor(released_day_of_week,
                        labels = c("Poniedziałek", "Wtorek", "Środa", "Czwartek", "Piątek", "Sobota", "Niedziela")), fill = factor(released_year))) +
  facet_wrap(~released_year, scales = "free") +
  geom_bar() +
  labs(title = "Rozkład liczby wypuszczonych piosenek względem dnia tygodnia",
       subtitle = "Od 2019 do 2022 roku",
       x = "Dzień tygodnia",
       y = "Liczba piosenek") +
  theme_bw()+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values = moje_kolory)+
  theme(legend.position = "none")
  

#KRÓTKI KOMENTARZ DO ZADANIA:

#Uznałem, że warto wykorzystać facet_wrap(). Użyłem
#wbudowanej funkcji weekdays z odpowiednim arg, żeby obliczyć dzień tygodnia. Wykrozystałęm tutaj również
#wykres słupkowy, bo dobrze obrazuje dane. Dodałem podpisy, kolory.
#Artyści najchętniej wypuszczali kawałki na początku weekendu, a zwłaszcza w sobotę.
#Tendencja ta utrzymywała się w każdym z roków.


# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?


spotify %>% 
  filter(streams > quantile(streams, 0.8, na.rm=TRUE)) %>%
  ggplot(aes(x = bpm)) +
  facet_wrap(~mode) +
  geom_density(alpha = 0.8, fill="green", color="black") +
  labs(title = "Rozkład tempa względem skali", 
       subtitle = "Dla piosenek w 20% najczęściej odtwarzanych piosenek",
       x = "Tempo",
       y = "Gęstość") +
  theme_bw()


#KRÓTKI KOMENTARZ DO ZADANIA:

#Dla skali minor tempo skupia się mniej więcej między 125 - 150,
#a dla skali major nieco bardziej się rozprasza. 

# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?


spotify %>% 
  group_by(artist.s._name) %>% 
  summarise(total_streams = sum(streams, na.rm = TRUE)) %>% 
  arrange(-total_streams) %>%
  mutate(artist.s._name = fct_reorder(artist.s._name, total_streams)) %>% 
  head(10) %>%
  ggplot(aes(x = artist.s._name, y = total_streams))+
    geom_segment(aes(x = artist.s._name, xend= artist.s._name, y=0, yend= total_streams), color= "black")+
    geom_point(color = "blue", size = 4)+
    coord_flip()+
    labs(y = "Zlogarytmowana liczba wszystkich odtworzeń", x = "Pseudonim artysty",
         title = "Top 10 artystyów z największą liczbą odtworzeń",
         subtitle = "Dane z platformy Spotify")+
    theme_light()+
    theme(panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank())+
    scale_y_continuous(expand = c(0, 0))


#KRÓTKI KOMENTARZ DO ZADANIA:

#Tym razem dla urozmaicenia moich wykresów, zdecydowałem się użyć tzw. Lollipop chart
#Jest on idealny do zmiennej jakościowej i ilościowej.
#Zmieniłem kolejność poziomów, aby artyści wyświetlali się w kolejności od najlpeszych
#w dół. 

  #Wynik:
  # 1 The Weeknd        14185552870
  # 2 Taylor Swift      14053658300
  # 3 Ed Sheeran        13908947204
  # 4 Harry Styles      11608645649
  # 5 Bad Bunny         9997799607
  # 6 Olivia Rodrigo    7442148916
  # 7 Eminem            6183805596
  # 8 Bruno Mars        5846920599
  # 9 Arctic Monkeys    5569806731
  # 10 Imagine Dragons  5272484650



# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

moje_kolory_2 <- c("red", "blue", "magenta", "green")

spotify %>% 
  filter(artist_count %in% c(1, 2, 3, 4)) %>% 
  ggplot(aes(x = energy_., y = danceability_., color = factor(artist_count)))+
    geom_point()+
    scale_color_manual(values = moje_kolory_2)+
    facet_wrap(~artist_count)+
    labs(x = "Energetyczność", y = "Taneczność", 
         title = "Wykres zależności energetyczności od taneczności piosenek",
         subtitle = "W zależności od ilości artystów")+
    theme(legend.position = "none")+
    geom_labelsmooth(aes(label = 'Trend'), fill = "white",
                     method = "lm", formula = y ~ x,
                     size = 3, linewidth = 1, boxlinewidth = 0.4) +
    theme_bw() + guides(color = 'none')
    
#KRÓTKI KOMENTARZ DO ZADANIA:

#W tym zadaniu postanowiłem użyć scatterplota. Podzieliłem wykres na 4 części, mimo
#iż jest łącznie 8 różych opcji ilości artystów. Zdecydowałem się na taki manewr, ponieważ
#dla 5,6,7 i 8 artystów jest mniej niż 10 piosenek i ciężko cokolwiek powiedzieć
#na temat zależności między tanecznością, a energetycznością.
#Analizując każdy z 4 wykresów wyraźnie widać, że albo taneczność wzrasta wraz z energetycznością
#lub utrzymuje się na podobnym poziomie. Nie widać wyraźnej korelacji między jednym, a drugim.
#Można to wytłumaczyć tym, że niektóre piosenki są bardzo energetyczne, ale mało taneczne
#np. heavy metalowe itp. Jedyna trudność tu była z tym, żeby wsadzić artist_count w factor()
#Dodałem od siebie linie trendu. Nie widać zależności ilości artystów od tanecznosci i energetyczności.
  

# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

options(scipen = 12)

spotify %>% 
  filter(released_year >= 2013 & released_year <= 2023) %>% 
  group_by(released_year) %>% 
  summarise(streams = (max(streams)), na.rm = TRUE) %>% 
  left_join(spotify, by = "streams") %>% 
  select(released_year.x, track_name, streams) %>% 
  mutate(track_name = fct_reorder(track_name, released_year.x), released_year.x = factor(released_year.x)) %>% 
  ggplot(aes(y = track_name, x = streams, fill = released_year.x))+
    geom_col()+
    geom_text(aes(x = 100000000, label = released_year.x), color = "black", size = 4)+
    labs(x = "Łączna liczba odtworzeń", y = "Tytuł utworu",
         title = "Najbardziej popularne utwory w latach 2013-2023",
         subtitle = "Dane z serwisu Spotify")+
    theme_classic()+
    theme(legend.position = "none")+
    theme(axis.text.y = element_text(family = "Times", size = 10))+
    scale_x_continuous(expand = c(0, 0))
  
    



#KRÓTKI KOMENTARZ DO ZADANIA:

#Najbardziej popularne piosenki:
#2013 Take Me To Church        
#2014 Thinking Out Loud         
#2015 Love Yourself             
#2016 One Dance                 
#2017 Shape of You              
#2018 Someone You Loved         
#2019 Blinding Lights           
#2020 Heat Waves                
#2021 STAY (with Justin Bieber) 
#2022 As It Was                 
#2023 Flowers                   



# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

spotify %>% 
  filter(released_year == 2022) %>%
  group_by(released_month) %>%
  summarise(in_spotify_playlists = sum(in_spotify_playlists, na.rm = TRUE),
            in_apple_playlists = sum(in_apple_playlists, na.rm = TRUE)) %>%
  pivot_longer(cols = c(in_spotify_playlists, in_apple_playlists)) %>% #tutaj musiałem aż do lab_2 sięgnąć
  ggplot(aes(x = factor(released_month, labels = c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec", "Lipiec", "Sierpień", "Wrzesień", "Październik", "Listopad", "Grudzień")), 
             y = value, color = name)) +
  geom_point(size=4) +
  labs(title = "Zależność pomiędzy sumą piosenek na playlistach spotify i apple w zależności od miesiąca",
       subtitle = "Dane na 2022 rok",
       x = "Miesiąc",
       y = "Suma piosenek na playlistach") +
  scale_color_manual(values = c("darkgrey", "green"),
                     name = "Platforma",
                     labels = c("Apple", "Spotify")) +
  theme_bw()

#KRÓTKI KOMENTARZ DO ZADANIA
#Apple przegrywa ze spotify. Z kretesem i bezpardonowo.





