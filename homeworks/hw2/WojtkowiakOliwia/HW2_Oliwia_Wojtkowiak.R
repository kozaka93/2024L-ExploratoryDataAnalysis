

spotify <- read.csv("C:/Users/oliwi/Downloads/spotify-2023.csv")

library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)


# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

number_of_artists<-spotify %>% 
  filter(released_year==2023, released_month==c(1,2,3))  %>% 
  mutate(streamsss = as.double(streams)) %>% 
  mutate(how_many_artists=as.factor(artist_count))%>%
  select(how_many_artists, streamsss)  


ggplot(number_of_artists, aes(x = streamsss , fill = how_many_artists)) + 
  geom_density(alpha=0.45) +
  labs(x = "Streams",
       y="Density",
       fill= "Number of artists",
       title = "Distribution of streams of songs depending on number of artists ",
       subtitle = "(considering songs released in january, february and march of 2023)")



# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

songs_released <- spotify %>% 
  filter(released_year==2019:2022) %>% 
  mutate(date_of_release = paste(released_year,released_month,released_day,sep="-")) %>% 
  mutate(new_dor = as.Date(date_of_release)) %>% 
  mutate(day_of_week = weekdays(new_dor)) %>%
  select(day_of_week,released_year,track_name) %>% 
  mutate(year = as.factor(released_year),days = factor(day_of_week,levels=c('poniedziałek','wtorek','środa','czwartek','piątek','sobota','niedziela')))%>% 
  group_by(year, days) %>%
  summarise(n = n())

ggplot(songs_released,aes(x = days,y = n,fill = year))+ 
  geom_col(position = "dodge") +
  labs(x ="Weekday",
       y="Number of songs released",
       fill= "Year",
       title = "Number of songs released in several weekdays in several years from 2019 to 2022.")
       






# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df3<-spotify%>% 
  mutate(Most=as.double(streams)/in_spotify_playlists)%>%
  filter(Most>=quantile(Most,0.8))%>%
  select(bpm,mode)  

ggplot(df3,aes(x=bpm, fill=mode))+ 
  geom_density(alpha=0.5)+ 
  labs(x = "Tempo",
       y="Gęstość",
       fill= "Skala",
       title = "Rozkład tempa względem skali dla piosenek.",
       subtitle = "(Dotyczy piosenek, które są w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbe playlist w spotify)")



# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

plays<-spotify%>% 
  filter(artist_count==1)%>%
  mutate(str=as.double(streams))%>%
  mutate(.by=artist.s._name,artist_stream=sum(str,na.rm=TRUE))%>%
  arrange(desc(artist_stream))%>%
  distinct(artist.s._name,artist_stream)%>%
  top_n(10,artist_stream)

ggplot(plays ,aes(x=reorder(artist.s._name, artist_stream ),y=artist_stream))+
  geom_col(fill='lightgreen')+
  labs(x = "Artists",
       y="Total songs stream",
       title = "Top 10 artists with most streaming time")



# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

energy<-spotify%>% 
  mutate(articount=as.factor(artist_count))%>% 
  select(articount ,danceability_.,energy_.)

ggplot(energy, aes(x=energy_.,y=danceability_.))+ 
  geom_smooth(method="lm", se = F, position = "jitter")+ 
  facet_wrap(~articount,scales = "free_x")+ 
  labs(x = "Perkiness",
       y="Danceability",
       title = "Dependence between perkiness and danceability of a song, depending on artists count.")


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?








# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?








