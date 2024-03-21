spotify <- read.csv("spotify-2023.csv")
library(dplyr)
library(ggplot2)
library(forcats)
library(lubridate)
library(tidyr)

#View(spotify)
#colnames(spotify)
# W zadaniach bedziemy korzystac tylko z wartosci streams jako liczb, a nie jako napisow, zmienmy je wiec na liczby
spotify$streams <- as.numeric(spotify$streams)


# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>% 
  filter(released_year == 2023 &
           released_month %in% c("1","2","3")) %>% 
  group_by(artist_count) %>% 
  ggplot(aes(x = factor(artist_count), 
             y = streams))+
  geom_boxplot(fill = "lightblue")+ 
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Rozklad odtworzen piosenek wedlug \n\ liczby artystow w 1 kwartale 2023 roku",
       x = "Liczba artystów",
       y="Liczba odtworzeń")

#widzimy ze najwiekszy rozrzut maja piosenki tworzone przez 2 artystow, najmniejszy przez 3. Najpopularniejsza jest jednak piosenka tworzona przez pojedynczego autora

# Zadanie 2 ---------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

spotify %>% 
  filter(released_year >= 2019,
         released_year <= 2022) %>%
  mutate(d_tyg = weekdays(
    as.Date(paste(released_year,
                  released_month,
                  released_day,
                  sep = "-")))) %>% 
    group_by(released_year,d_tyg) %>% 
  summarise(suma = n()) %>% 
  ggplot(aes(x = factor(released_year),
             y = suma,
             fill = d_tyg))+
    geom_col(position = "dodge") +
    labs(title="Rozklad liczby wypuszczonych piosenek \n\ wedlug dni tygodnia dla lat 2019 - 2023 ",
         x = "Rok", 
         y = "Liczba utworów", 
         fill = "Dzień tygodnia") 

# jest ewidentnie widoczny trend wypuszczania piosenek w piatek lub w 2 dni przed piatkiem. Sobota i niedziela sa dniami o najmniejszej liczbie wypuszczonych piosenek
  
# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

spotify %>% 
  filter(in_spotify_playlists>=quantile(
    in_spotify_playlists, probs = 4/5)) %>% 
  group_by(mode) %>% 
  ggplot(aes(x=mode, y=bpm, fill = mode)) +
  geom_violin() +
  labs(title = "Rozkład tempa względem skali dla \n\ piosenek z top 20% najczęściej \n\ znajdujących się na playlistach Spotify",
       x = "",
       y = "Tempo w uderzeniach na minute",
       fill = "Skala")+
  ylim(0,220)+
  theme(axis.text.x = element_blank())

#utwory ze skala Minor sa mniej rozrzucone na osi tempa niz piosenki ze skali Major. Dla tempa okolo 120 bpm ewidentnie widac, ze piosenek ze skali Minor jest najwiecej, dla skali Major jest to okolo 108bpm

# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

# oba kody sa dobre
#
# spotify %>%
#   filter(artist_count == 1) %>%
#   group_by(artist.s._name) %>%
#   summarize(suma = sum(streams)) %>%
#   arrange(-suma) %>%
#   head(10) %>%
#   mutate(artist.s._name = fct_reorder(
#   artist.s._name, suma)) %>%
#   ggplot(aes(x = artist.s._name,
#              y = suma,
#              fill = artist.s._name)) +
#   geom_col()+
#   labs(title = "top 10 artystow o nawiekszej sumie odsluchan wszystkich wlasnych utworow",
#        x = "artysci")+
#   scale_y_continuous(labels = scales::comma)+
#   theme(axis.text.x = element_blank())

spotify %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarize(suma = sum(streams, na.rm = T)) %>% 
  mutate(artist.s._name = fct_reorder(
    artist.s._name, suma)) %>% 
  arrange(-suma) %>% 
  head(10) %>% 
  ggplot(aes(x = artist.s._name, 
             y = suma, 
             fill = artist.s._name)) +
  geom_col()+
  labs(title = "Top 10 artystow o nawiekszej sumie \n\ wszystkich odsluchan wlasnych utworow",
       x = "",
       y = "Suma odsluchan",
       fill = "Artyści")+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_blank())
  
# tutaj wykres oraz ta tabelka z dokladnymi warosciami mowia same za siebie
# 1 The Weeknd      14185552870
# 2 Taylor Swift    14053658300
# 3 Ed Sheeran      13908947204
# 4 Harry Styles    11608645649
# 5 Bad Bunny        9997799607
# 6 Olivia Rodrigo   7442148916
# 7 Eminem           6183805596
# 8 Bruno Mars       5846920599
# 9 Arctic Monkeys   5569806731
# 10 Imagine Dragons  5272484650

# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>% 
  ggplot(aes(x = danceability_.,
             y= energy_., 
             color = factor(artist_count))) +
  geom_point(size = 1.6)+
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#17becf"))+
  labs(title = "Zależnosć energetyczności utworu od \n\ jego taneczności zależnie od liczby artystów",
       x = "Taneczność",
       y = "Energetyczność",
       color = "Liczba artystów")

#wszystko sie skupia wokol okolo punktu (70,70), im wiecej artystow, tym wiekszy rozrzut

# Zadanie 6 ----------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

spotify %>% 
  filter(released_year >= 2013, 
         released_year <= 2023) %>% 
  transform(track_name = ifelse(track_name == "STAY (with Justin Bieber)", "STAY (ft. J. Bieber)", track_name)) %>%
  group_by(released_year) %>% 
  top_n(1, streams) %>% 
  ggplot(aes(x = factor(released_year), 
             y = streams,
             label = track_name)) +
  geom_col(fill = "black")+
  geom_text(position = position_dodge(width = 0),    
            vjust = 0.3,
            hjust = 1.1,
            size = 5,                               
            angle = 90,             
            color = "lightblue",                
            aes(group = track_name)) + 
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Najpopularniejsze piosenki w latach \n\ 2013 - 2023 wraz z liczba ich odtworzen",
       x = "Rok",
       y = "Liczba odtworzeń")

#tutaj wykres mowi sam za siebie, wstawie tabelke z dokladnymi wartosciami
# released_year    streams track_name          
# 1   2023 1316855716 Flowers             
# 2   2022 2513188493 As It Was           
# 3   2021 2665343922 STAY (ft. J. Bieber)
# 4   2020 2557975762 Heat Waves          
# 5   2019 3703895074 Blinding Lights     
# 6   2018 2887241814 Someone You Loved   
# 7   2017 3562543890 Shape of You        
# 8   2016 2713922350 One Dance           
# 9   2015 2123309722 Love Yourself       
# 10  2014 2280566092 Thinking Out Loud   
# 11  2013 2135158446 Take Me To Church

# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?


spotify %>% 
  filter(released_year == 2022) %>% 
  group_by(released_month) %>% 
  summarise(spot = sum(in_spotify_playlists),
            apple = sum(in_apple_playlists)) %>% 
  mutate(month = month.name[released_month]) %>%
  transform(month = fct_reorder(month, released_month)) %>% 
  ggplot(aes(x = spot,
             y = apple, 
             color = factor(month))) +
  geom_point(size = 4)+
  geom_abline(intercept = 550, slope = 0.01 , color = "black", linetype = "dashed") +
  scale_color_manual(values =  c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#aec7e8", "#ffbb78"))+
  labs(x = "Suma piosenek na playlistach spotify",
       y = "Suma piosenek na playlistach apple",
       title = "Suma piosenek na playlistach spotify vs \n\ apple patrząc po miesiącach w 2022 roku",
       color = "Miesiąc wydania")+
  xlim(0,210000)+
  ylim(0,2600)


#widzmy skupienie wokol jednej prostej
# f(x) = 550 +0.01x
# ewidentnym outlierem zarowno dla appla i spotify jest majm, reszta miesiecy skupiona jest raczej niedaleko punktu (75000, 1300)