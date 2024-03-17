library(dplyr)
library(ggplot2)
library(tidyr)
library(stringi)
spotify <- read.csv("spotify-2023.csv")
spotify <-spotify %>% mutate(streams=as.numeric(spotify$streams))
options(scipen = 100000000)
# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

df1<-spotify %>% 
  filter(released_year==2023 & released_month==1:3)

df1 %>% ggplot(aes(streams,color=as.factor(artist_count)))+geom_density()+
  labs(x="liczba wyświetleń",y="gęstość",
       title = "Rozkład liczby odtworzeń piosenek zależności od liczby wykonawców",
       subtitle = "Pierwszy kwartał 2023 roku",color="Liczba wykonawców:")+
  scale_x_continuous(breaks =seq(0,max(df1$streams),by=200000000),expand = c(0,0))


# Dla jednego wykonawcy przeważająca jest liczba wyswietleń pomiędzy 100 a 250 tysięcy.
# Poza tym istnieją dwa mniejsze piki w okolicach 500 tysięcy i 1,3 mld wyświetleń, 
# ponadto jest bardzo niewiele piosenek poza tymi przedziałami.
# W przypadku dwóch wykonawców przeważający udział mają piosenki z liczbą wyświetleń w okolicach 150 tysięcy.
# Nastepnie udział w dość regularny sposób maleje wraz ze wzrostem liczby wyświetleń i zanika przy ok. 900 tysiącach wyświetleń.
# Dla trzech wykonawców isteniają dwa piki w okolicach 100 i 200 tysięcy. 
# Następnie następuje bardzo szybki zanik w okolicach 300 tysięcy 
# (bardzo niewiele piosenke powyżej 300 tysięcy wyświetleń dla trzech autorów).  





# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

df2<-spotify %>% 
  filter(released_year>=2019 & released_year<=2022) %>% 
  mutate(weekday=weekdays(as.Date(paste(released_year,released_month,released_day,sep="-"),format="%Y-%m-%d"))) %>% 
  group_by(released_year,weekday) %>% 
  summarise(n=n())

df2 %>% ggplot(aes(x = weekday, y =n,fill=as.factor(released_year))) +
  geom_col()+ scale_x_discrete(labels = c("poniedziałek","wtorek","środa","czwartek","piątek","sobota","niedziela"))+
  labs(y="Liczba piosenek",x="Dzień tygodnia",fill="Rok wydania",
       title = "Liczba wypuszczonych piosenek w poszczególnych dniach tygodnia",subtitle = "w latach 2019-2022")+
  scale_fill_manual(values=c("yellow","green","blue","red"))

# Widać, że niezależnie od roku najczęściej piosenki wypuszczano w środę, drugim dniem był poniedziałek,
# a następnym sobota (przy czym w 2019 w sobotę wydano mniej piosenek niż we wtorek). 
# Kolejnymi dniami są niedziela i czwartek, przy czym w czwartek wydano więcej piosenek w 2020 roku,
# a w niedziele w 2021. Najmniej piosenek wydano we wtorek i w piątek. 
# Piątek jest też jedynym dniem, gdzie wydano w każdym roku poza 2019 podobną liczbę piosenek 
# (w każdym innym dniu znaczna przewaga roku 2022).



# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df3<-spotify %>% 
  mutate(srednia=streams/in_spotify_playlists) %>% 
  filter(srednia>=quantile(srednia,0.8,na.rm=T))

df3 %>% ggplot(aes(x=bpm,y=mode))+geom_boxplot()+
  stat_summary(fun = "mean", geom = "point", shape = 19, size = 3, color = "red")+
  scale_x_continuous(breaks =seq(0,max(df3$bpm),by=25))+labs(y="Mode",x="Tempo",title="Rozkład tempa względem względem skali 'mode'",
                    subtitle = "Dla 20 % najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify")

# W przypadku kategorii Minor jak i Major wartość średnia jak i mediana mają bardzo podobne wartości 
# i w obu przypadkach oscylują w okolicach 125 (dla Major minimalnie większa). W przypadku Minor wartość
# najmnijesza jest osiągana w pobliżu 75, zaś dla Major w pobliżu 70. Największą wartością tempa 
# dla Minor jest 200, zaś dla Major ok. 190. W przypadku Minor mamy krótszą "skrzynię" co oznacza,
# że większość danych jest skupina wokół średniej, jedank istnieją dwa outliery dla dużych wartości.
# W przypadku Major mamy doczynienie z bardziej równomiernym rozłożeniem wartości, o czym świadczy 
# wydłużona "skrzynia" i brak outlierów.



# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

df4<-spotify%>%
  mutate(artist.s._name = stri_split_fixed(artist.s._name, ", ")) %>%
  unnest(artist.s._name) %>% 
  group_by(artist.s._name) %>% 
  summarise(suma=sum(streams)) %>% 
  arrange(-suma) %>% 
  head(10)

df4 %>% ggplot(aes(x=reorder(artist.s._name,-suma),y=suma)) + geom_col()+labs(y="Suma odtworzeń", x="Artysta",
title="Top 10 artystów z największą liczbą odtworzeń")

# Kolejno: The Weeknd, Bad Bunny, Ed Sheeran, Taylor Swift, Harry Styles, 	
# Eminem, Dua Lipa, Justin Bieber, Drake, BTS. Zespół z największą liczbą odtworzeń ma ich ok. 2,4 mld,
# zaś żeby znaleźć się w pierwszej 10 trzeba było ich mieć co njamniej ok. 800 mln.



# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?


ggplot(spotify, aes(x = danceability_., y = energy_.,color=artist_count)) + 
  geom_point()+labs(x="Taneczność",y="Energetyczność",color="Liczba artystów:",title="Zależność energetyczności od taneczności",
                    subtitle = "z uwzględnieniem liczby artystów")+
  geom_smooth()+scale_color_gradient(low="lightblue",high="navy")

# Linia trendu wskazuje, że enrgetyczność danego utworu wzrasta wraz ze wzrostem taneczności 
# osiągając średnio ok. 65 dla 80 punktów taneczności. Dalszy wzrost taneczności powoduje już 
# spadek enrgetyczności do poziemu ok. 60. Największa część piosenek jest zlokalizowana w przedziale 
# (60,80) dla taneczności i (50,75) dla energetyczności, przy czym daje się zauważyć, że jeżeli
# wykonawców danego utwor jest więcej niż 1 to jego taneczność i energetyczność będą średnio większe.




# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

df6<-spotify %>% 
  filter(released_year>=2013 &released_year<=2023) %>%   
  group_by(released_year) %>% 
  slice(which.max(streams))

df6 %>% ggplot(aes(x=as.factor(released_year),y=streams))+geom_point(color="red")+geom_line(group=1,color="red")+
  labs(x="Rok",y="Liczba odtworzeń",title="Najbardziej popularna piosenka w danym roku",subtitle="lata 2013-2023")+
  scale_y_continuous(breaks =seq(0,max(df6$streams),by=500000000))+geom_text(aes(label = track_name))

# Są to odpowiednio kolejno: Take Me To Church, Thinking Out Loud, Love Yourself,
# One Dance, Shape of You, Someone You Loved, Blinding Lights, Heat Waves, STAY (with Justin Bieber),
# As It Was, Flowers. Ponadto możemy porównać popularność poszeczególnych piosenek 
# i zobaczyć że największą popularność bez podziału na lata osiągnął Bliding Lights (ponad 3,7 mld), a najmniejszą Flowers.




 # Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

df7<-pivot_longer(spotify %>% 
  filter(released_year==2022) %>% 
  group_by(released_month) %>% 
  summarise(Spotify=log(sum(in_spotify_playlists)),Apple=log(sum(in_apple_playlists)))
  ,col=-released_month,names_to="sp_app",values_to = "sum")

df7 %>% ggplot(aes(x=as.factor(released_month),y=sum,fill=sp_app))+geom_col(position = "dodge")+
  labs(x="Miesiąc",y="Suma piosenek (skala log)",title="Porównanie sumy piosenek na playlistach na spotify i apple",
       subtitle="w poszczególnych miesiącach w 2022 roku",fill="Rodzaj playlisty")

# W każdym miesiącu widać zdecydowaną przewagę spotify nad apple. Z wykresu widać ogólną zależność, 
# że więcej piosenek na spotify implikuje większą liczbę piosenek na apple. 
# Widać to choćby po tym, że miesiące z największą i najmniejszą liczbą pioenek są takie same dla obu playlist
# (odpowiednio maj i wrzesień)
# Naturalną implikacją tej zależności jest fakt, że różnica pomiędzy liczbą piosenek na obu playlistach jest podobna.
# Jedynym miesiącem wyraźniej odstającym od tej zależności jest luty, gdzie mamy "za dużo" piosenek z apple 
# i "za mało" ze spotify.




