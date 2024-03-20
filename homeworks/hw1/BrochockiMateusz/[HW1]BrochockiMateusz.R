library(dplyr)

data <- read.csv("data.csv")


# Zadanie 0 ---------------------------------------------------------------
# Jaka marka samochodu najczesciej wystepuje w zbiorze danych?

data %>%
  group_by(Make) %>%
   summarise(n = n())%>%
  arrange(-n)

# Odp.: Chevrolet   


# Zadanie 1 ---------------------------------------------------------------
# Jaka jest mediana mocy silnika dla samochodów w zależności od rodzaju
# skrzyni biegów?
data %>%
  group_by(Transmission.Type) %>%
  summarise(Median_power = median(Engine.HP,na.rm=TRUE)) 


# Odp.:
#1 AUTOMATED_MANUAL     220 HP
#2 AUTOMATIC            253 HP
#3 DIRECT_DRIVE         147 HP
#4 MANUAL               172 HP
#5 UNKNOWN              125 HP


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?
data %>%
  filter(Make=="Mercedes-Benz"& Engine.Fuel.Type=="diesel") %>%
 summarise(iqr_city.mpg = IQR(city.mpg,na.rm=TRUE)) 


# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>%
  filter(Engine.Cylinders==4) %>%
  group_by(Make) %>%
  summarise(avg_price = mean(MSRP,na.rm=TRUE)) %>% 
  arrange(-avg_price)


# Odp.: Alfa Romeo    61600 


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>%
  filter(Make=="BMW") %>%
  group_by(Driven_Wheels) %>% 
  summarise(n = n())%>%
  arrange(-n)

data %>%
  filter(Make=="BMW" &  Driven_Wheels=="rear wheel drive") %>%
  group_by(Model) %>% 
  summarise(n = n())%>%
  arrange(-n)

# Odp.: Najwięcej jest aut z napędem tył (189), na drugim miejscu są z napędem na 4 (144), 
#a najmniej z napędem na przód (1). Najczęściej występujący model dla aut z napędem na tył to 1 Series.


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>%
  filter(Make=="Volkswagen"&Number.of.Doors==c(2,4)) %>%
  group_by(Number.of.Doors) %>% 
  summarise(Median_power = median(Engine.HP))

# Odp.: Dla dwudrzwiowych wynosi 170 HP, a dla czterodrzwiowych 200 HP.


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.
data %>%
  mutate(city.mpg_100km_L=235.214/city.mpg) %>% 
  group_by(Make,Year) %>%  
  summarise(avg = mean(city.mpg_100km_L)) %>% 
  arrange(-avg)

# Odp.: 2008 i 2009 Bugatti 29.4 L/100km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
data %>%
  filter(2007<=Year&Year<=2017) %>%
  group_by(Vehicle.Style,Year) %>% 
  summarise(Mean_popularity_by_year = mean(Popularity,na.rm=TRUE)) %>% 
  group_by(Vehicle.Style) %>% 
  summarise(Mean_popularity = mean(Mean_popularity_by_year,na.rm=TRUE)) %>% 
  arrange(-Mean_popularity)
  
data %>%
  filter(Year>=2007&Year<=2017) %>%
  group_by(Vehicle.Style,Year) %>% 
  summarise(Mean_popularity_by_year = mean(Popularity,na.rm=TRUE)) %>% 
  group_by(Year) %>% 
  slice(which.max(Mean_popularity_by_year)) %>% 
  group_by(Vehicle.Style) %>% 
  summarise(n=n())

# Odp.: W latach 2007-2017 najbardziej popularny był Passenger Van.
# Najczęściej  wystąpiły Cargo Minivan i Cargo Van (po 3 razy).    


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?
data %>%
  filter(Year>=2000 & Market.Category	=="Luxury,Performance") %>%
  group_by(Make) %>% 
  summarise(Median_MSPR = median(MSRP	,na.rm=TRUE),Mean_Engine.Cylinders=mean(Engine.Cylinders,na.rm = TRUE),Mean_Engine.HP=mean(Engine.HP,na.rm=TRUE)) %>% 
  arrange(-Median_MSPR) %>% 
  slice(1,n())


# Odp.: Volkswagen Różnica w średniej liczbie cylindrów to 4.7, a w mocy 86 HP.  


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>%
  filter (Transmission.Type=="AUTOMATIC" & Vehicle.Size	=="Midsize")%>%
  summarise(Q=quantile(city.mpg,c(0.1,0.9),na.rm=TRUE))

# Odp.: 0.1 kwantyl =  14; 0.9 kwantyl= 25 (mpg)



# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>% 
  filter(Make=="Porsche" & Engine.HP<=300)  %>% 
  group_by(Model) %>% 
  summarise(Mean_popularity=mean(Popularity))
 

# Odp.: Wszystkie modele spełniające warunki tj. 718 Cayman, 944, 968, Boxster, Cayenne, Cayman, Cayman S, Macan
# mają taką samą popularność, a więc nie istnieje model o drugiej największej popularności.


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>% 
  mutate(highway.mpg_100km_L=235.214/highway.MPG) %>% 
  group_by(Engine.Fuel.Type	,Driven_Wheels) %>% 
  summarise(Median_HP=median(Engine.HP,na.rm=TRUE),Mean_highway.MPG=mean(highway.mpg_100km_L)) %>% 
  arrange(-Median_HP)


# Odp.: flex-fuel (premium unleaded required/E85) all wheel drive  12.3 L/100


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>% 
  mutate(diffrence=abs(235.214/city.mpg-235.214/highway.MPG)) %>% 
  group_by(Make,Model) %>% 
  summarise(Mean_diff=mean(diffrence,na.rm=TRUE)) %>% 
  arrange(-Mean_diff)

# Odp.:  Ferrari Enzo 14.0 L/100 

