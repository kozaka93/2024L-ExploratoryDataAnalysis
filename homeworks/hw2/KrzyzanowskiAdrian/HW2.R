spotify <- read.csv("spotify-2023.csv")
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>% filter(released_year==2023 & released_month<=3) %>%
  mutate(odtworzenia=as.numeric(streams)) %>% 
  group_by(artist_count) %>% summarise(liczba_odtworzeń=sum(odtworzenia)) %>% 
  ggplot(aes(x=artist_count, y=liczba_odtworzeń)) +
  geom_col(fill="dodgerblue3", color="black") +
  scale_y_continuous(labels = scales::number_format(scale=1e-9)) +
  labs(x="Liczba wykonawców", y="Liczba odtworzeń (w miliardach)", title="Odtworzenia a liczba wykonawców",
       subtitle="w I kwartale 2023") +
  theme(plot.title = element_text(face = "bold"))
  
#Komentarz: Z wykresu widać, że najwięcej odtworzeń osiągały piosenki wykonane przez 
#2 artystów, niewiele mniej przez 1 artystę, najmniej (znacznie) przez 3 artystów.



# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?


spotify %>%
  filter(released_year<=2022 & released_year>=2019) %>% 
  mutate(rok=as.character(released_year), miesiac=as.character(released_month), dzien=as.character(released_day)) %>% 
  mutate(miesiac0=ifelse(nchar(miesiac)==1, paste("0", miesiac, sep = ""), miesiac)) %>% 
  mutate(nap_data=paste(rok, miesiac0, dzien, sep="")) %>% 
  mutate(weekday=weekdays(as.Date(nap_data, format="%Y%m%d"))) %>% 
  ggplot(aes(x=weekday, fill=rok)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(limits = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela"),
                   guide=guide_axis(angle=30)) +
  labs(x="Dzień tygodnia", y="Liczba wypuszczonych piosenek", title="Dni tygodnia a ilość wypuszczonych piosenek",
       subtitle="w latach 2019-2022") +
  theme(plot.title = element_text(face = "bold"))

#Komentarz: W każdym roku najwięcej piosenek było wypuszczonych w piatek. Najmniej
# to już zależy od roku np. w 2019 najmniej było w sobotę, a w 2020 we wtorek (0).
#W każdym dniu tygodnia wygrywa 2022 pod względem ilości piosenek.


# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?


spotify %>% 
  filter(track_name!="Love Grows (Where My Rosemary Goes)") %>% 
  mutate(przeliczenie=as.numeric(streams)/in_spotify_playlists) %>% 
  arrange(-przeliczenie) %>% 
  top_n(n=0.2*n(), wt=przeliczenie) %>% 
  ggplot(aes(x=mode, y=bpm)) +
  geom_boxplot(fill="yellowgreen") +
  labs(x="Skala", y="Tempo (bpm)", title="Tempo względem skali",
       subtitle="dla 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist") +
  theme(plot.title = element_text(face = "bold")) +
  scale_y_continuous(breaks = seq(75, 200, by = 25)) +
  stat_summary(fun = mean, geom = "point", size = 2.5, color = "firebrick2")
  
#Komentarz: Można zauważyć, że dla skali Major rozkład jest bardziej rozciągnięty, czyli
#ma większy IQR. Średnia oraz mediana mają podobne wartości dla obu skal (około 125 bpm)
#Skala Minor ma dwa outliery.




# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

#rozdzielenie artystów

spotify_mod <- spotify %>% 
   select(track_name, artist.s._name, streams)

for (i in 1:953) {
  if (spotify[i, 3]>1){
    for (j in 2:spotify[i,3]){
      spotify_mod<-add_row(spotify_mod, track_name=spotify[i, 1], artist.s._name=str_split(spotify[i, 2], ", ")[[1]][j], streams=spotify[i, 9])
    }
    spotify_mod[i, 2]<- str_split(spotify[i, 2], ", ")[[1]][1]
  }
}

spotify_mod %>% filter(track_name!="Love Grows (Where My Rosemary Goes)") %>% 
  mutate(streamy=as.numeric(streams)) %>% 
  group_by(artist.s._name) %>% summarise(ile=sum(streamy)) %>% 
  arrange(-ile) %>% head(10) %>%
  ggplot(aes(x=reorder(artist.s._name, ile), y=ile))+
  geom_col(fill= "purple2", color="black") +
  scale_y_continuous(labels = scales::number_format(scale=1e-9)) +
  labs(x="Wykonawca", y="Suma odtworzeń (w miliardach)", title="Artyści a odtworzenia",
       subtitle="10 największych") +
  theme(plot.title = element_text(face = "bold")) +
  scale_x_discrete(guide=guide_axis(angle=30))

  
#Komentarz: Najwięcej odtworzeń sumarycznie miał The Weekend, natomiast niewiele gorzej
#wypadł Bad Bunny (obydwoje powyżej 23 mld). Podium zamknął Ed Sheeran z ponad 15 mld 
#odtworzeń.



# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?


spotify %>% 
  mutate(ile_art=as.character(artist_count)) %>% 
  ggplot(aes(x=danceability_., y=energy_., color=ile_art))+
  geom_point() +
  scale_color_manual(values=c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d','#666666')) +
  labs(x="Taneczność", y="Energetyczność", title="Taneczność a energetyczność", 
       color="Liczba artystów") +
  theme(plot.title = element_text(face = "bold")) +
  geom_smooth(method="lm", se=FALSE, linewidth=0.8)

#Komentarz: Widzimy, że jest jakaś korelacja pomiędzy tanecznością a energetycznością,
#dla 1 i 3 artystów trochę większa, a dla 2 i 4 mniejsza. Natomiast dla większej ilości
#artystów jest za mało danych, żeby cokolwiek wnioskować (już dla 4 jest niewiele)



# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w
#latach 2013 - 2023?


spotify %>% filter(released_year>=2013 & released_year<=2023) %>% 
  mutate(odtw=as.numeric(streams)) %>% group_by(released_year, track_name) %>% 
  reframe(liczba_odtw=odtw) %>% 
  group_by(released_year) %>% 
  filter(liczba_odtw==max(liczba_odtw)) %>% 
  ggplot(aes(x=released_year, y=liczba_odtw, fill=reorder(track_name, released_year))) +
  geom_col() +
  scale_x_continuous(breaks = seq(2013, 2023, by = 1), guide=guide_axis(angle=30)) +
  labs(x="Rok", y="Liczba odtworzeń (w miliardach)", title="Najpopularniejsze piosenki", 
       fill="Piosenka") +
  theme(plot.title = element_text(face = "bold")) +
  scale_y_continuous(labels = scales::number_format(scale=1e-9))
  

#Komentarz: Która piosenka była najpopularniejsza w danym roku to widać w legendzie. 
#Natomiast najpopularniejsze ze wszystkich było Blinding Lights z 2019 roku z ponad 
#3,5 mld odtworzeń. Najpoularniejsza piosenka z 2023 (Flowers) ma najmniej odtworzeń
#spośród najpopularniejszych piosenek w tych latach 




# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify
#a apple patrząc po miesiącach?


spotify %>% filter(released_year==2022) %>% 
  mutate(released_month2=as.character(released_month)) %>% 
  group_by(released_month2) %>% 
  summarise(Spotify=sum(in_spotify_playlists), Apple=sum(in_apple_playlists)*40) %>% 
  pivot_longer(cols = c(Spotify,Apple), names_to = "Platforma", values_to = "Suma") %>% 
  ggplot(aes(x=released_month2, y=Suma, fill=Platforma)) +
  geom_col(position = "dodge", color="black") +
  scale_y_continuous(sec.axis = sec_axis(trans = ~./40, name = "Suma na playlistach Apple")) +
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")) +
  labs(x="Miesiąc", y="Suma na playlistach Spotify", title="Piosenki na playlistach Spotify i Apple") +
  theme(plot.title = element_text(face = "bold"))

#Komentarz: Prawie w każdym miesiącu jest taka zależność, że jak na Spotify jest większa
#suma w porównaniu z poprzednim miesiącem, to również na Apple jest większa suma z wyjątkiem
#2-go miesiąca, czyli Lutego. Widzimy też, że na Apple suma piosenek na playlistach jest
#znacznie mniejsza niż na Spotify, dlatego potrezbna była druga skala. Podobnie na obu
#platformach największa suma jest maju, a najmniejsza we wrześniu.

