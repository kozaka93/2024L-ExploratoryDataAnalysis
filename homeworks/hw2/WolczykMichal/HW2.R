library(ggplot2)
library(dplyr)
getwd
spotify <- read.csv("spotify-2023.csv")
poprawne <- spotify %>% 
  mutate(wyswietlenia = as.integer(streams))
# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

#Zdecydowałem pokazać to boxplotami bo dużo pokazują kiedy mamy doczynienia z jedną zmienną,
# druga zmienna przyjmuje tylko 3 wartości więc po niej grupuje

spotify %>% 
  filter(released_year == 2023 & released_month>0 & released_month < 4) %>% #na początku filtruje dane do wykresu
  ggplot(aes(x=as.factor(artist_count), y = as.numeric(streams))) + geom_boxplot()+
  labs( x = 'liczba_artystów',
        y = 'ilość odtworzeń',
        title = 'Rozkład liczby odtworzeń w 2023 w pierwszym kwartale')
#użyłem as.numeric(), żeby uciąglić streamsy, potem standardowe działanie z ggplotem

#Jak widać piosenki z 2 artystami częściej mają więcej wyświetleń, aczkolwiek piosenki z jednym
#artystą mają kilku outlyerów
  





# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

#Tutaj uznałem że użyje stacked barplota, barploty dobrze pokazują ilość, a jako że mamy dwie zmienne grupujące
#to sam barplot wyglądałby średnio
spotify %>% 
  filter(released_year >= 2019 & released_year <=2022) %>% 
  mutate(Dzien = weekdays(as.Date(paste(released_year,released_month,released_day,sep = '-')))) %>% 
  ggplot(aes(x= Dzien, fill = as.factor(released_year))) + geom_bar(position = 'dodge') +
  labs(y = "Ilość wypuszczonych piosenek",
       fill = "Rok wydania",
       title = "W którym dniu tygodnia było wypuszczane najwięcej piosenek?")
#jako że geom_bar sam zlicza wystąpienia, to rok dałem jako wypełnienie aby nim też pogrupować


#Widzimy, że zdecydowana większość piosenek jest wydawana w piątek, daleko za piatkiem troche wybija się jeszcze czwartek




# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

#Znowu uznałem że boxplot będzie najlepszy do zwizualizowania danych
#Najpierw obliczam kwantyl
kwantyl_20 = quantile(poprawne$in_spotify_playlists,probs = 0.8, na.rm = TRUE)
#potem filtruje na jego podstawie
spotify %>% 
  filter(streams>=kwantyl_20) %>% 
  ggplot(aes(x=mode,y= bpm)) + geom_boxplot() +
  labs(x = "skala",
       y= 'tempo',
       title = "rozkład tempa względem sklai piosenek w top 20% odtworzeń")
#Piosenki w skali Major mają troche wyższe tempo, niż piosenki w skali minor, jednakże
#ich mediana nie różnią się za bardzo






# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

#Tutaj uznałem że najlepszy będzie wykres punktowy
popularni <- poprawne%>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarise(suma_odtworzeń = sum(wyswietlenia)) %>% 
  arrange(-suma_odtworzeń)
  
ggplot(popularni[1:10,], aes(x = artist.s._name, y = suma_odtworzeń)) +
  geom_point() +
  labs(x = "Artysta", 
       y = "Suma odtworzeń",
       title = "TOP 10 Artystów pod względem sumy odtworzeń piosenek")

#Widzimy że Taylor swift zdecydowanie jest na pierwszym miejscu

# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

#tutaj znowu wykres punktowy

spotify %>% 
  ggplot(aes(x=energy_., y = danceability_., color = as.factor(artist_count))) +geom_point() +
  labs(x = "energetyczność",
       y = "taneczność",
       color = "liczba artystów")

#widzimy że generalnie nie ma za bardzo piosenek które są mało energetyczne a bardzo taneczne i odwrotnie




# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

# tutaj uznałem że wykres punktowy będzie najlepszy
df <- poprawne %>% 
  filter(released_year %in% seq(2013,2023))
df1 <- df %>% 
  group_by(released_year) %>% 
  summarise(max = max(wyswietlenia, na.rm = TRUE)) 
poprawne %>% 
  filter(wyswietlenia %in% df1$max) %>% 
  ggplot(aes(x = as.factor(released_year), y = wyswietlenia)) +geom_point() +
  geom_text(aes(label = track_name)) +labs(x = "wypuszczony rok",
                                           title = 'Najpopularniejsze piosenki w poszczególnym roku')
#Użyłem geom text żeby podpisać punkty na wykresie





# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

#tutaj też wykres punktowy dobrze to pokazuje
spotify %>% 
  filter(released_year == 2022) %>% 
  group_by(released_month) %>% 
  summarise(spotify_play = sum(in_spotify_playlists), apple = sum(in_apple_playlists)) %>% 
  ggplot(aes(x = as.factor(released_month))) + geom_point(aes(y = spotify_play), color = 'green') +
  geom_point(aes(y= apple), color = 'blue')+labs(x = 'miesiąc',
                                                 y= 'w ilości playlist')
#Tu widać że spotify występuje po prostu więcej razy.







