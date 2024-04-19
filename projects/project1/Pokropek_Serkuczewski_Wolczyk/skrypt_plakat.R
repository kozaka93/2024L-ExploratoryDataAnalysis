install.packages("ggplot2")
install.packages("maps")
install.packages("mapdata")

library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(forcats)

nowy_kolor <- rgb(0,0,1,alpha = 0.75)
movies <- read.csv("movies.csv")
world <- map_data("world")

doMapyZRatingiem <- movies%>%
  filter(country != "")%>%
  group_by(country)%>%
  summarise(countOfMovies = n(),AvgRating= mean(score))%>%
  select(country,countOfMovies,AvgRating) %>% 
  mutate(country = ifelse(country=="United States","USA",country)) %>% 
  mutate(country = ifelse(country=="United Kingdom","UK",country)) 

joinedMapa <- world%>%left_join(doMapyZRatingiem, by = c("region" = "country"))%>%
  mutate(countOfMovies = ifelse(is.na(countOfMovies), 0, countOfMovies)) %>% 
  mutate(logarytm = log(countOfMovies)) %>% 
  mutate(logarytm = ifelse(logarytm <0, 0, logarytm))
  
#troche głupie ale wychodzi ze brazylijskie kino to wyzsza klasa
joinedMapa %>% 
  filter(countOfMovies>1000) %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(data = joinedMapa, aes(fill = AvgRating), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  scale_fill_gradient(low = 'darkred',high=  'green')


joinedMapa%>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(data = joinedMapa, aes(fill = logarytm), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  scale_fill_gradient(low = 'darkred',high=  'green')

#mamy lata od 1980 - 2020; zaleznosc ilości wydawanych filmów od średniej oceny filmow wydawanych w latach 1980 - 2020; tu chyba lepiej dac barplota  x = rok, y = srednia, a na slupkach podpis z iloscia filmow
zaleznoscIloscFilmowOdLat <- movies%>%
  group_by(year)%>%
  summarise(countOfMovies = n(), avgRating = mean(score,na.rm = TRUE))%>%
  ggplot(aes(x = year, y = avgRating, color = as.factor(countOfMovies))) +
  geom_point() +
  labs(x = "Rok",
       y = "Średnia ocena",
       title = "zaleznosc ilości wydawanych filmów od średniej oceny filmów na przestrzeni lat") + 
  theme_bw() 

dlugoscfilmowwzaleznosciodlat <- movies %>% 
  group_by(year) %>% 
  summarise(srdl = mean(runtime, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = srdl)) + geom_bar(stat = 'identity')

gwiazdy <- movies %>% 
  group_by(star) %>% 
  summarise(ilosc_filmow = n(), avg_rating = mean(score), poczatek_kariery = min(year), koniec_kariery = max(year)) %>% 
  mutate(dlugosc_kariery = koniec_kariery - poczatek_kariery+1) 
  
#całkiem logiczne wnioski że im dłuższa kariera to wyższe oceny
gwiazdy_podzielone <- gwiazdy %>% 
  filter(ilosc_filmow >1) %>% 
  mutate(jak_dlugo = case_when(dlugosc_kariery<=5 ~ "0-5",
                               dlugosc_kariery <= 10 ~ "6-10",
                               dlugosc_kariery <= 15 ~ "10-15",
                               dlugosc_kariery <= 20 ~ "15-20",
                               TRUE ~">20"
                               )) 
gwiazdy_podzielone %>% 
  ggplot(aes(x=jak_dlugo,y = avg_rating)) +geom_bar(stat = 'summary', fun = 'mean')

elita <- gwiazdy_podzielone %>% 
  filter(jak_dlugo == ">20") %>% 
  left_join(movies, by = "star") %>% 
  mutate(moment_kariery = year - poczatek_kariery)
elita  %>% 
  ggplot(aes(x= moment_kariery, y = score)) +geom_point(stat = 'summary', fun = 'mean')

Polska <-movies %>% 
  filter(country == "Poland") 
  


akcja_czy_komedia <- movies %>% 
  filter(genre %in% c("Action","Comedy")) %>% 
  mutate(a_c_k = case_when(genre == "Action"~1, genre == "Comedy"~0)) %>% 
  group_by(country) %>% 
  summarise(czego_wiecej = sum(a_c_k)/n()) %>% 
  mutate(decyzja = case_when(czego_wiecej<0.5 ~"comedy", TRUE ~ "action"))

 
a_c_k <- world%>%left_join(akcja_czy_komedia, by = c("region" = "country"))

a_c_k %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(data = a_c_k, aes(fill = decyzja), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  scale_fill_manual(values = c("pink","blue"))

  
  
#w jakich panstwach powstały srednio najlepiej oceniane konkretne gatunki filmow; w polsce top animacje top top kozak polska gurom

movies%>%
  filter(country != "")%>%
  group_by(country, genre)%>%
  summarise(avgRating = mean(score, na.rm = TRUE))%>%
  group_by(country)%>%
  filter(avgRating == max(avgRating))%>%
  group_by(genre)%>%
  filter(avgRating == max(avgRating))
  

#top 10 par gwiazda - rezyser cieszacych sie najlepsza srednia ocena filmow dla vote >= 100k

movies%>%
  filter(star != "" & director != "" & votes>=100000)%>%
  group_by(star, director)%>%
  summarise(avgRating = mean(score, na.rm = TRUE),ilosc_filmow_razem = n())%>%
  filter(ilosc_filmow_razem > 2) %>% 
  arrange(-avgRating)%>%
  head(10)

#czy wielkosc budzetu powoduje ze film ma dobre recenzje? top 10 filmow z najlepszymi recenzjami w latach 1980-2000, chyba kwartyle???
  
movies%>%
  filter(year %in% seq(1980,2000))%>%
  
  
#Jak ilość ocen wpływa na średnią filmu?
  
doocen <-movies %>% 
  mutate(grupy =ceiling(log(votes,10))) %>% 
  mutate(grupy_1 = case_when(grupy == 2~1, grupy == 7~6, TRUE~grupy))
doocen$grupy_1 <- factor(doocen$grupy_1, levels = c(1,3,4,5,6),labels = c("<=1 tys.","(1 tys.-10 tys.]","(10 tys.-100 tys.]","(100 tys.-1 milion]",">1 milion"))  
doocen <- doocen[complete.cases(doocen$grupy_1),]
doocen %>% 
  group_by(grupy_1) %>% 
  summarise(srednia_ocen = mean(score, na.rm = TRUE)) %>% 
  ggplot(aes(x=grupy_1,y=srednia_ocen,fill = ''))+geom_bar(stat = 'identity')+
  theme_dark()+
  theme(plot.background =  element_rect('black'), axis.text = element_text(color = 'white'),axis.title.y = element_text(color = 'white'),
        title = element_text(color = 'white'),
        plot.title = element_text(hjust = 0.35))+
  labs(x = "Ilość ocen",y="Średnia ocena",title = "Zależność średniej oceny od ilości ocen")+
  scale_fill_manual(values = nowy_kolor)+
  theme(legend.position = 'none')+
  geom_text(aes(label = round(srednia_ocen,digits = 1)),vjust = 1.5, size = 7)
 
  
  
  
  
  
  
  
writers <- movies %>% 
  group_by(writer) %>% 
  summarise(ilosc_filmow = n(), avg_rating = mean(score), poczatek_kariery = min(year), koniec_kariery = max(year)) %>% 
  mutate(dlugosc_kariery = koniec_kariery - poczatek_kariery +1) 

directors <- movies %>% 
  group_by(director) %>% 
  summarise(ilosc_filmow = n(), avg_rating = mean(score), poczatek_kariery = min(year), koniec_kariery = max(year)) %>% 
  mutate(dlugosc_kariery = koniec_kariery - poczatek_kariery+1)


gwiazdy_wiel <- gwiazdy %>% 
  filter(ilosc_filmow>1) %>% 
  mutate(Filmy_na_rok = ilosc_filmow/dlugosc_kariery) %>% 
  mutate(Grupa = "Aktor")
writers_wiel <-writers %>% 
  filter(ilosc_filmow>1) %>% 
  mutate(Filmy_na_rok = ilosc_filmow/dlugosc_kariery) %>% 
  mutate(Grupa = "Scenarzysta")
directors_wiel <- directors %>% 
  filter(ilosc_filmow>1) %>% 
  mutate(Filmy_na_rok = ilosc_filmow/dlugosc_kariery) %>% 
  mutate(Grupa = "Reżyser")
ggplot() +
  geom_boxplot(data = gwiazdy_wiel,aes(x =Grupa,y = 1/Filmy_na_rok ,fill = "")) +
  geom_boxplot(data = directors_wiel,aes(x =Grupa,y = 1/Filmy_na_rok ,fill = "")) +
  geom_boxplot(data = writers_wiel,aes(x =Grupa,y = 1/Filmy_na_rok ,fill = "")) +
  facet_grid(. ~ Grupa, scales = "free_x", space = "free_x")+
  labs(x="Grupa",
        y = "Częstotliwość projektów filmowych",
        title = "Co ile lat artysta bierze udział w filmie?")+
  theme_dark()+
  scale_fill_manual(values = nowy_kolor)+
  theme(legend.position = 'none')+
  theme(plot.background =  element_rect('black'), axis.text = element_text(color = 'white'),axis.title.y = element_text(color = 'white'),
        title = element_text(color = 'white'),axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = seq(0, 20, by = 2.5), n.breaks = 10)