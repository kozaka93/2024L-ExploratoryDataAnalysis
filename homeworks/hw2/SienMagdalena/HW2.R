spotify <- read.csv("spotify-2023.csv")
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(RColorBrewer)
library(scales)
options(warn=-1)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>% 
  filter(released_year==2023 & released_month %in% c(1,2,3) & !is.na(as.double(streams))) %>% 
  ggplot(aes(x=factor(artist_count), y=as.double(streams)))+
  geom_violin(fill='coral4')+
  labs(title='Liczba odtworzeń piosenek opublikowanych
  w pierwszym kwartale 2023 roku w zależności od liczby wykonawców',
  x='liczba wykonawców', y='liczba odtworzeń')

# Wnioski: Najwięcej odtworzeń piosenek jest dla 1 i 2 wykonawców. Niezależnie od
# ilości artystów mediana ilości odtworzeń jest na podobnym poziomie. Dla 3 
# artystów liczba odtworzeń jest najbardziej skupiona.
  
# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

spotify %>% 
  filter(released_year<=2022 & released_year>=2019) %>% 
  mutate(day=weekdays(as.Date(str_c(released_year,released_month,released_day,sep='-')))) %>%
  group_by(day) %>% 
  ggplot(aes(x=day))+
  geom_bar(fill='darkred') + 
  facet_wrap(~released_year,scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title.y=element_blank())+
  scale_x_discrete(limits=c('poniedziałek','wtorek','środa','czwartek','piątek','sobota','niedziela'))+
  labs(title='Liczba wypuszczonych piosenek względem dnia tygodnia w latach 2019-2022',
       x='dzień tygodnia')

# Wnioski: Bez względu na rok najwięcej piosenek było wypuszczanych w piątki. 
# Na drugim miejscu zawsze był czwartek. Rozkład reszty dni był różny zależnie 
# od roku. Liczba wypuszczonych piosenek rosła wraz z rokiem.

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

# Wnioski: Mediana dla obu skali jest zbliżona (około 130 bpm). Dla skali Major 
# rozkład jest bardziej zbity, a dla skali Minor jest więcej odstających 
# wartości.

# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

  s<-{spotify %>% 
  separate_rows(artist.s._name, sep = ", ") %>%
  group_by(artist.s._name) %>% 
  filter(!is.na(as.double(streams))) %>% 
  mutate(sum=sum(as.double(streams))) %>% 
  arrange(-sum) %>% 
  select(artist.s._name, sum) %>% 
  distinct() %>% 
  head(10)}
s<-as.data.frame(s)
s %>% 
  ggplot(aes(reorder(artist.s._name,sum),sum))+
  geom_bar(stat="identity",fill='coral4')+
  labs(title='Top 10 artystów z największą ilością odtworzeń piosenek', 
       x='artysta', y='liczba odtworzeń')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Wnioski: Najwięcej odtworzeń mają kolejno: The Weekend, Bad Bunny, Ed Sheeran,
# Taylor Swift, Harry Styles, Eminem, Dua Lipa, Justin Bieber, Drake, BTS.
  

# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>% 
  ggplot(aes(energy_.,danceability_.))+
  geom_point(color='darkred')+facet_wrap(~artist_count, ncol=2)+
  labs(title="Zależność energetyczności od tanecznośći według liczby artystów",
       x='energetyczność',y='taneczność')

# Wnioski: Wartości skupiają się w prawej górnej ćwiartce. Trudno określić 
# jednoznacznie, jaka zależnosć występuje. Można się chylić do stwierdzenia, 
# że taneczność rośnie wraz z energetyczością.

# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

 spotify %>% 
  filter(released_year<=2023 & released_year>=2013 & !is.na(as.double(streams))) %>% 
  mutate(streams=as.double(streams)) %>% 
  group_by(released_year) %>% 
  arrange(released_year,desc(streams)) %>% 
  top_n(1,as.double(streams)) %>% 
  select(released_year,track_name,streams) %>% 
  mutate(year=factor(released_year)) %>% 
  ggplot(aes(y=year, x=streams))+
  geom_bar(stat="identity",fill='coral4')+
  scale_x_continuous(labels=label_number())+
  geom_text(aes(x=streams*0.8,label=track_name))+
  labs(title="Najbardziej popularne piosenki w latach 2013 - 2023", x='liczba odtworzeń', y='rok')

# Wnioski: Najbardziej popularne piosenki w latach: 2013 - Take Me To Church,
# 2014 - Thinking Out Loud, 2015 - Love Yourself, 2016 - One Dance, 
# 2017 - Shape of You, 2018 - Someone You Loved, 2019 - Blinding Lights,
# 2020 - Heat Waves, 2021 - STAY, 2022 - As It Was, 2023 - Flowers
# Najbardziej popularne z tych piosenek były: Blinding Lights i Shape of You.

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
 
 # Wnioski: Suma piosenek na playlistach apple w porównaniu do playlist spotify 
 # bez względu na rok była zawsze znacznie niższa. Największa suma dla obu 
 # aplikacji była w maju, a najmniejsza we wrześniu.

