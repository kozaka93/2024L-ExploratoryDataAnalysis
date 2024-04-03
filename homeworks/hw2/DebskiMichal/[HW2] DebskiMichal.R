spotify <- read.csv("spotify-2023.csv")

library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

df1<-spotify %>% 
  filter(released_year==2023, released_month==c(1,2,3))  %>% 
  mutate(Stream = as.double(streams)) %>% 
  mutate(Artists=as.factor(artist_count))%>%
  select(Artists,Stream)  

ggplot(df1,aes(x= Stream, fill=Artists)) + 
  geom_density(alpha=0.5) +
  labs(x = "Liczba odtworzeń",
       y="Gęstość",
       fill= "Liczba wykonawców",
       title = "Rozkład liczby odtworzeń piosenek w zależności od liczby wykonawców.",
       subtitle = "(Dotyczy piosenek opublikowanych w 2023 roku w pierwszym kwartale)")

# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

#?weekdays

df2<-spotify%>% 
  filter(released_year==2019:2022) %>% 
  mutate(Date1=paste(released_year,released_month,released_day,sep="-")) %>% 
  mutate(Date=as.Date(Date1)) %>% 
  mutate(Weekday=weekdays(Date)) %>%
  select(Weekday,released_year,track_name) %>% 
  mutate(Year=as.factor(released_year),days=factor(Weekday,levels=c('poniedziałek','wtorek','środa','czwartek','piątek','sobota','niedziela')))%>% 
  group_by(Year,days) %>%
  summarise(n=n())

ggplot(df2,aes(x=days,y=n,fill=Year))+ 
  geom_col(position = 'dodge') +
  labs(x ="Dzień tygodnia",
     y="Liczba wypusczonych piosenek",
     fill= "Rok",
     title = "Liczba wypuszczonych piosenek w poszczególnych dniach tygodnia w poszczególnych latach.",
     subtitle = "(Dotyczy piosenek opublikowanych w latach: 2019-2022)")

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

df4<-spotify%>% 
  filter(artist_count==1)%>%
  mutate(STREAMS=as.double(streams))%>%
  mutate(.by=artist.s._name,Total_streams=sum(STREAMS,na.rm=TRUE))%>%
  arrange(desc(Total_streams))%>%
  distinct(artist.s._name,Total_streams)%>%
  top_n(10,Total_streams)

ggplot(df4,aes(x=reorder(artist.s._name,Total_streams),y=Total_streams))+
  geom_col(fill='lightblue')+
  labs(x = "Artyści",
       y="Łączne odtworzenia piosenek",
       title = "Top 10 artystów z największą liczbą wyświetleń piosenek.")

# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?


df5<-spotify%>% 
  mutate(AC=as.factor(artist_count))%>% 
  select(AC,danceability_.,energy_.)

ggplot(df5,aes(x=energy_.,y=danceability_.))+ 
  geom_smooth(method="lm",se = F)+ 
  facet_wrap(~AC,scales = "free_x")+ 
  labs(x = "Energetyczność",
       y="Taneczność",
       title = "Zależność między energetycznością, a tanecznością.",
       subtitle="(W zależności od ilości artystów)")


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

df6<- spotify%>%
  filter(released_year==2013:2023)%>%
  mutate(Stream=as.double(streams))%>%
  mutate(.by=released_year,Max=max(Stream,na.rm=T))%>% 
  filter(Max==Stream) %>%
  select(released_year,track_name,Max)%>%
  arrange(released_year)


ggplot(df6, aes(x=reorder(track_name,released_year),y=Max,fill=as.factor(released_year))) + 
  geom_col() + 
  labs(x = "Nazwa piosenki",
       y="Liczba odtworzeń",
       title = "Najpopularniejsze piosenki w latach 2013-2023.",
       fill="Rok")
  

# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

# 1. przypadek 
df7<- spotify%>% 
  filter(released_year==2022) %>% 
  select(released_month,in_spotify_playlists,in_apple_playlists)%>%
  arrange(released_month) %>% 
  mutate(released_month=as.factor(released_month))

ggplot(df7,aes(x=in_spotify_playlists,y=in_apple_playlists)) + 
  geom_smooth(se=F) + 
  facet_wrap(~released_month,scales="free_x") + 
  labs(x = "playlisty na spotify",
       y="playlisty na apple",
       title = "Zależność pomiędzy sumą piosenek na playlistach na spotify a apple.",
       subtitle="(Rok 2022, dla poszczególnych miesięcy)")

# 2. przypadek 
df8<- spotify%>% 
  filter(released_year==2022) %>% 
  mutate(.by=released_month, sptf=sum(in_spotify_playlists,na.rm=T),app=sum(in_apple_playlists,na.rm=T)) %>%
  distinct(released_month, sptf, app)

ggplot(df8,aes(x=sptf,y=app,color=as.factor(released_month))) + 
  geom_point()+ 
  labs(x = "playlisty na spotify",
       y="playlisty na apple",
       color="Miesiące",
       title = "Zależność pomiędzy sumą piosenek na playlistach na spotify a apple.",
       subtitle="(Rok 2022, dla poszczególnych miesięcy)")


