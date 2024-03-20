spotify <- read.csv("spotify-2023.csv")
library(ggplot2)
library(dplyr)
library(forcats)
library(lubridate)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify%>%
  filter(released_year==2023, released_month %in% c(1,2,3))%>%
  group_by(artist_count)%>%
  mutate(streams=as.numeric(streams))%>%
  ggplot(aes(x=factor(artist_count), y=streams/1000000, color=artist_count))+
  geom_boxplot(color=c('#4b015e', 'purple', 'hotpink'))+
  labs(title='Rozkład liczby odtworzeń w pierwszym kwartale 2023r.',
       x='Liczba wykonawców',
       y='Liczba odtworzeń w milionach')

#Na wykresie widać, że średnia ilość odtworzeń jest podobna dla wszystkich liczb wykonawców
#Najbardziej zbliżone do siebie liczby odtworzeń mają grupy trzech artustów
#Najbardziej zróżnicowane są dla duetów




# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?


spotify%>%
  filter(released_year>=2019, released_year<=2022)%>%
  mutate(data = make_date(released_year,released_month, released_day))%>%
  mutate(dni_tygodnia=factor(weekdays(data), levels=c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela")))%>%
  group_by(dni_tygodnia, released_year)%>%
  summarise(ilosc_piosenek=n())%>%
  ggplot(aes(x=released_year,fill=dni_tygodnia, y=ilosc_piosenek))+
  geom_bar(stat='identity', position = 'dodge')+
  labs(title = 'Rozkład wypuszczonych piosenek w latach 2019-2022',
       x='Liczba piosenek',
       y='Rok wydania',
       fill='Dni tygodnia')

#Z wykresu wynika, że najczęściej piosenki są wypuszczane w piątki
#oraz liczba wypuszczanych piosenek wraz z latami wzrasta




# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

spotify%>%
  mutate(przeliczenie=as.numeric(streams)/as.numeric(in_spotify_playlists))%>%
  arrange(-przeliczenie)%>%
  head(nrow(spotify)*0.2)%>%
  group_by(mode)%>%
  ggplot(aes(x=mode,y=bpm))+
  geom_violin(fill='hotpink')+
  labs(title='Rozkład tempa dla 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify',
       x='Skala mode',
       y='Tempo (bpm)')

#Rozkład tempa Minor jest ma mniejszą gęstość w górnej połowie wykresu, podczas gdy Major jest bardziej symetryczny
#Oba mają największe skupienie w tym samym miejscu (okolicach 125)




# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?



spotify%>%
  filter(artist_count==1)%>%
  group_by(artist.s._name)%>%
  summarise(suma=sum(as.numeric(streams), na.rm=TRUE)/1000000000)%>%
  arrange(-suma)%>%
  head(10)%>%
  ggplot(aes(x=fct_reorder(artist.s._name,suma), y=suma, fill=artist.s._name))+
  geom_bar(stat='identity', fill=c('#00aaff','#aa00ff','#ff00aa','#ffaa00','#aaff00','#3d144c' ,'#1685f8','#e900ff','#f52789','#faeb2c'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = 'Top 10 artystów z największą ilością odtworzeń',
       x='Artysta',
       y='Suma odtworzeń w milionach')
  
#Suma odtworzeń na początku wykresu wzrasta powoli, ale od 'Bad Bunny' szybciej się zwięsza
#Również widać dużą różnicę między artystami, z dalszych miejsc a top 3, którzy mają dwukrotnie większą liczbę odtworzeń





# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify%>%
  group_by(artist_count)%>%
  ggplot(aes(x=danceability_., y=energy_., color=as.factor(artist_count)))+
  geom_point()+
  scale_color_manual(values =c('#ff00a9', '#00aaff', 'red', '#be29ec','#800080','#faeb2c','#03396c', 'black'))+
  labs(title = 'Zależność energetyczności od taneczności',
       x='Taneczność',
       y='Energetyczność',
       color='Liczba artystów')

#Widocznie dominującymi liczbami artystów są soliści oraz duety
#Na wykresie widać, że jest mało utworów a niską tanecznością oraz z niską energetycznością
#Największe skupienie jest w prawej, górnej ćwiartce



# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?



spotify%>%
  filter(released_year>=2013, released_year<=2023)%>%
  mutate(streams=as.numeric(streams))%>%
  group_by(released_year)%>%
  top_n(1, streams)%>%
  mutate(streams=streams/1000000000)%>%
  mutate(track_name=fct_reorder(track_name, released_year))%>%
  ggplot(aes(x=track_name, y=streams, fill=track_name))+
  geom_bar(stat='identity',fill=c('#00aaff','#aa00ff','#ff00aa','#ffaa00','#aaff00','#3d144c' ,'#1685f8','#e900ff','#f52789','#faeb2c', 'red'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title='Najbardziej popularne piosenki w latach 2013-2023',
       x='Nazwa piosenki',
       y='Odtworzenia w miliardach')
  
#Piosenki, któe były wypouszczone stosunkowo niedawno mają mniej odtworzeń, tak samo jak starsze piosenki
#Najwięcej odtworzeń mają piosenki wyouszczone w latach 2017-2019
#Może być to spowodowane wzrostem popularności spotify w tych latach
#Rownież nowe piosenki nie miały wystarczająco czasu, aby uzbierać większą liczbę odtworzeń




# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

spotify%>%
  filter(released_year==2022)%>%
  group_by(released_month)%>%
  summarise(na_spotify=sum(in_spotify_playlists), na_apple=sum(in_apple_playlists))%>%
  ggplot(aes(x=na_spotify, y=na_apple, color=as.factor(released_month)))+
  geom_bar(stat = identity)+
  scale_color_manual(values = c('#00aaff', '#aa00ff', '#ff00aa', '#ffaa00', '#aaff00', '#3d144c', '#1685f8', '#e900ff', '#f52789', '#faeb2c', 'red', 'black')) +
  labs(title = 'Zależność pomiędzy sumą piosenek na playlistach',
       x = 'Spotify',
       y = 'Apple',
       color = 'Miesiąc wydania')+
  xlim(0,210000)+
  ylim(0,2600)

# Widocznie w maju jest najwięcej piosenek na playlistach
# poza tym widać, że suma na spotify i apple music jest proporcjonalna




