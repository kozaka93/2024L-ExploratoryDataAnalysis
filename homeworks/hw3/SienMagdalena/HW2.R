spotify <- read.csv("spotify-2023.csv")
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(RColorBrewer)
options(warn=-1)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>% 
  filter(released_year==2023 & released_month %in% c(1,2,3) & !is.na(as.double(streams))) %>% 
  group_by(artist_count) %>% 
  mutate(sum=sum(as.double(streams))) %>% 
  ggplot(aes(x=artist_count, y=sum))+
  geom_bar(stat="identity",fill='coral4')+
  labs(title='Liczba odtworzeń piosenek opublikowanych
  w pierwszym kwartale 2023 roku w zależności od liczby wykonawców',
  x='liczba wykonawców', y='liczba odtworzeń')+
  
# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

spotify %>% 
  filter(released_year<=2022 & released_year>=2019) %>% 
  mutate(day=weekdays(as.Date(str_c(released_year,released_month,released_day,sep='-')))) %>%
  group_by(day) %>% 
  mutate(n=n()) %>% 
  ggplot(aes(x=day, y=n))+
  geom_bar(stat="identity",fill='darkred') + 
  facet_wrap(~released_year,scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title.y=element_blank())+
  scale_x_discrete(limits=c('poniedziałek','wtorek','środa','czwartek','piątek','sobota','niedziela'))+
  labs(title='Liczba wypuszczonych piosenek względem dnia tygodnia w latach 2019-2022',
       x='dzień tygodnia')

# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

spotify %>% 
  filter(!is.na(as.double(streams))) %>% 
  mutate(topka=(as.double(streams)/in_spotify_playlists)) %>% 
  arrange(-topka) %>% 
  top_frac(0.2) %>% 
  ggplot(aes(bpm,mode,fill=mode))+
  geom_violin(show.legend = FALSE)+
  labs(title='Rozkład tempa względem skali dla najczęściej odtwarzanych piosenek', x='tempo',y='skala')+
  theme(legend.title=element_blank())+
  scale_fill_manual(values = c('orchid','powderblue'))

# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

  spotify %>% 
  separate_rows(artist.s._name, sep = ", ") %>%
  group_by(artist.s._name) %>% 
  filter(!is.na(as.double(streams))) %>% 
  mutate(sum=sum(as.double(streams))) %>% 
  arrange(-sum) %>% 
  select(artist.s._name) %>% 
  distinct() %>% 
  head(10)

# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>% 
  ggplot(aes(energy_.,danceability_.))+
  geom_point(color='darkred')+facet_wrap(~artist_count, ncol=2)+
  labs(title="Zależność energetyczności od tanecznośći według liczby artystów",
       x='energetyczność',y='taneczność')

# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

spotify %>% 
  filter(released_year<=2023 & released_year>=2013 & !is.na(as.double(streams))) %>% 
  group_by(released_year) %>% 
  arrange(released_year,desc(streams)) %>% 
  top_n(1,streams) %>% 
  select(released_year,track_name)

# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

 s<-{
  spotify %>%
  filter(released_year==2022) %>% 
  mutate(month=month.abb[released_month]) %>% 
  group_by(month) %>% 
  mutate(suma=sum(in_apple_playlists), sums=sum(in_spotify_playlists)) %>% 
  arrange(released_month) %>% 
  select(month,suma,sums) %>% 
  distinct()}
 s<-as.data.frame(s)
 month<-rep(s$month,2)
 values<-c(s$suma,s$sums)
 category<-rep(c('apple', 'spotify'),each = nrow(s))
 d<-data.frame(month,values,category)
 d %>% 
   ggplot(aes(month,values,fill=category))+
   geom_bar(position="dodge", stat="identity",)+
   labs(title='Number of songs on apple and spotify playlists in 2022 by month')+
   theme(axis.title.y=element_blank(),legend.title=element_blank())+
   scale_x_discrete(limits=month[1:12])+ 
   scale_fill_manual(values = c('coral4','cornflowerblue'))
   





