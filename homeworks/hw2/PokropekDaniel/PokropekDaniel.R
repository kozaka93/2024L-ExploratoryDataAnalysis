spotify <- read.csv("/home/pop/Pobrane/spotify-2023.csv")

library(dplyr)
library(ggplot2)
library(tidyr)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>% 
  filter(released_year==2023 & released_month %in% 1:3 )%>% 
  ggplot(aes(y=as.factor(artist_count),x=as.numeric(streams))) + geom_violin()+
  labs(
    x='liczba odtworzeń',
    y='liczba wykonawców',
    title='Zadanie 1'
  )

# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

spotify %>%
  filter(released_year %in% 2019:2022) %>%
  mutate(dzien_tyg = weekdays(as.Date(paste(released_year, released_month, released_day, sep = "-")))) %>%
  group_by(dzien_tyg, released_year) %>% 
  summarise(liczba_piosenek = n()) %>% 
  ggplot(aes(y=liczba_piosenek,x=dzien_tyg, color=as.factor(released_year)))+geom_point(alpha = 0.7)+
  labs(
    x = "dzień tygodnia",
    y = "liczba piosenek",
    title = "Zadanie 2"
  )+scale_color_manual(values = c( "orange", "darkgreen", "blue", "red"))+ 
  theme(legend.title = element_blank())


# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

spotify %>% 
  arrange(in_spotify_playlists)%>% 
  slice_tail(prop=0.2) %>% 
  ggplot(aes(x=bpm,y=mode))+geom_violin()+
  labs(
    y = "skala",
    title = "Zadanie 3"
  )

# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

#w przypadku gdy jedną piosenkę tworzy więcej niż jeden artysta rozdzielam ten wiersz przy użyciu
#funkcji separate_longer_delim z pakietu tidyr(tak zrozumiałem ten nawias 'osobno' z polecenia)
#i wyswietlenia tego kawałka liczę dla każdego kto go współtworzył

spotify %>% 
  tidyr::separate_longer_delim(artist.s._name, delim = ", ") %>%
  group_by(artist.s._name) %>% 
  summarise(n_odtworzen=sum(as.numeric(streams), na.rm = TRUE)) %>% 
  arrange(desc(n_odtworzen)) %>% 
  head(10) %>%
  mutate(artist.s._name = forcats::fct_reorder(artist.s._name, desc(n_odtworzen))) %>% 
  ggplot(aes(x= artist.s._name, y=n_odtworzen/10^9))+geom_col()+
  labs(
    x = "artyści",
    y = "suma odtworzeń wszystkich piosenek w miliardach",
    title = "Zadanie 4"
  )

# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>% 
  mutate(en_tan_ratio=energy_./danceability_.) %>% 
  ggplot(aes(x=en_tan_ratio,y=as.factor(artist_count)))+geom_violin()+
  labs(
    x = "stosunek energetyczności od tanecznośći",
    y = "liczba artystów",
    title= "Zadanie 5"
  )

# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

wynik<-data.frame()
for(i in 2013:2023){
  
  spotify %>% 
    filter(released_year==i) %>% 
    mutate(streams=as.numeric(streams)) %>% 
    arrange(desc(streams))->lista
  
  wynik[i-2012,1:3]<-lista[1,c(1,2,4)]
}

# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

spotify %>% 
  filter(released_year==2022 & in_spotify_playlists!=0 & in_apple_playlists!=0) %>% 
  mutate(ratio=in_spotify_playlists/in_apple_playlists,
         miesiac = months(as.Date(paste(released_year, released_month, released_day, sep = "-")), abbreviate=TRUE)) %>%
  mutate(miesiac = forcats::fct_reorder(miesiac, desc(released_month))) %>% 
  ggplot(aes(y=miesiac,x=ratio))+geom_violin()+
  labs(
    x = "stosunek sumy piosenek na playlistach spotify a apple",
    y = "miesiąc",
    title="Zadanie 7"
  )
