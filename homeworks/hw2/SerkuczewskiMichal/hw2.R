library(dplyr)
library(ggplot2)
library(forcats)
library(RColorBrewer)
spotify <- read.csv("spotify-2023.csv")

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców? 

spotify%>%
  filter(released_year==2023 & released_month %in% seq(1,3))%>%
  mutate(streams = strtoi(streams))%>%
  group_by(artist_count)%>%
  summarise(total_streams = sum(streams))%>%
  select(artist_count, total_streams)%>%
  mutate(artist_count = as.character(artist_count))%>%
  mutate(artist_count = fct_reorder(artist_count,total_streams))%>%
  ggplot(aes(x=artist_count,y=total_streams))+
  geom_col()+
  labs(x = "Ilość odtworzeń", y = "Ilość wykonawców")


# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

zad2 <- spotify%>%
  filter(released_year %in% seq(2019,2022))%>%
  mutate(weekday = weekdays(as.Date(paste(released_year,released_month,released_day,sep = '-'))))%>%
  mutate(streams = strtoi(streams))%>%
  filter(!is.na(streams))%>%
  group_by(released_year,weekday)%>%
  mutate(streams_by_day = sum(streams,na.rm = TRUE))%>%
  select(released_year,released_day,weekday)%>%
  ggplot(aes(x= weekday, fill = as.factor(released_year))) + geom_bar(position = 'dodge') +
  labs(y = "Ilość wypuszczonych piosenek", fill = "Rok wydania")

# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spospotify

spotify%>%
  mutate(streams = strtoi(streams))%>%
  filter(
    streams >= quantile(
    (spotify%>%
       mutate(streams = strtoi(streams)))$in_spotify_playlists,,probs = 0.8, na.rm = TRUE
  ))%>%
  ggplot(aes(x = mode,y = bpm)) + 
  geom_boxplot() +
  labs(x = "skala", y = 'tempo')

# podczas filtrowania wykorzystujemy funkcje quantile a nastepnie tworzymy wykres


# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?


spotify%>%
  filter(artist_count==1)%>%
  mutate(streams=strtoi(streams))%>%
  group_by(artist.s._name)%>%
  summarise(total_streams=sum(streams,na.rm = TRUE))%>%
  mutate(artist.s._name = fct_reorder(artist.s._name,total_streams))%>%
  head(10)%>%
  ggplot(aes(x=artist.s._name,total_streams))+geom_col()

# filtrujemy aby miec osobnych artystow, i zmieniamy kolejnosc na podstawie nazw wykonawcow


# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify%>%
  ggplot(aes(x=energy_., y = danceability_., color = as.factor(artist_count))) +
  geom_point()

# prosta zaleznosc x - energetycznosc, y - tanecznosc, kolory punktow odpowiadaj ilosci artystow. wykres po powiekszeniu jest czytelniejszy.

# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

spotify%>%
  filter(released_year %in% seq(2013,2023))%>%
  mutate(streams = strtoi(streams))%>%
  group_by(released_year)%>%
  filter(streams == max(streams,na.rm = TRUE))%>%
  mutate(track_name = fct_reorder(track_name,released_year))%>%
  select(track_name,released_year,streams)%>%
  ggplot(aes(x = as.factor(released_year),y = streams))+
  geom_point()+
  geom_text(aes(label=track_name))

##filtruje lata sprawdzajac. czy sa. w zakresie 2013/23, korzystam z. geom_point aby moc dodattkowo podpisac piosenki



# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

spotify%>%
  filter(released_year == 2022)%>%
  group_by(released_month)%>%
  summarise(sum_spotify = sum(in_spotify_playlists, na.rm = TRUE), sum_apple = sum(in_apple_playlists, na.rm = TRUE))%>%
  ggplot(aes(x = as.factor(released_month)))+
  geom_point(aes(y = sum_spotify), color = 'green')+
  geom_point(aes(y = sum_apple), color = 'red')
  

#dodaje punkty zielony i czerwone kolejno odppowiadajace ilosci playlist spotify i apple do ktorych zostaly dodane utwoorryy



