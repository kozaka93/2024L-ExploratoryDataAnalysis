library(dplyr)
data <- read.csv("data.csv")


# Zadanie 0 ---------------------------------------------------------------
# Jaka marka samochodu najczesciej wystepuje w zbiorze danych?

data %>%
  group_by(Make) %>%
  summarise(n = n()) %>%
  arrange(-n)

# Odp.: Chevrolet   


# Zadanie 1 ---------------------------------------------------------------
# Jaka jest mediana mocy silnika dla samochodów w zależności od rodzaju
# skrzyni biegów?
data %>% 
  select(Engine.HP, Transmission.Type)%>% 
  mutate(.by=Transmission.Type, Median_engine_hp = median(Engine.HP,na.rm=TRUE))%>%
  distinct( Transmission.Type,Median_engine_hp)

# Odp.: MANUAL: 172
# AUTOMATIC:  253
# AUTOMATED_MANUAL: 220
# DIRECT_DRIVE: 147
# UNKNOWN:  125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?
data %>% 
  select(Make,Engine.Fuel.Type,city.mpg) %>% 
  filter(Make=="Mercedes-Benz" & Engine.Fuel.Type=="diesel") %>% 
  mutate(Iqr_city_mpg=IQR(city.mpg,na.rm=TRUE))%>%
  select(Iqr_city_mpg)%>%
  distinct(Iqr_city_mpg)

# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?
data%>%
  filter(Engine.Cylinders == 4) %>% 
  mutate(.by=Make, Mean_msrp=mean(MSRP,na.rm=TRUE)) %>%
  select(Make,MSRP,Mean_msrp) %>% distinct (Make,Mean_msrp)%>%
  arrange(desc(Mean_msrp)) %>% head(1)


# Odp.: Alfa Romeo; 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
data%>%
  select(Make,Driven_Wheels)%>%
  filter(Make=="BMW")%>% 
  group_by(Driven_Wheels)%>%
  summarise(count_driven_wheels=n())
data%>%
  select(Make,Model,Driven_Wheels)%>%
  filter(Make=="BMW" & Driven_Wheels=="rear wheel drive")%>% group_by(Model)%>%
  summarise(count_rear_wheels=n()) %>% top_n(1,count_rear_wheels)
  
# Odp.: Najwięcej jest aut BMW z napędem na tył. I najczęściej występującym modelem z napędem na tył jest BMW 1 Series. 


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?
data %>% 
  filter(Make== "Volkswagen" & (Number.of.Doors==2 |Number.of.Doors==4))%>%
  mutate(.by=Number.of.Doors, Median_engine_hp = median(Engine.HP,na.rm=TRUE)) %>% 
  select(Number.of.Doors,Median_engine_hp) %>% distinct(Number.of.Doors,Median_engine_hp)
  


# Odp.:Jesli samochod posiada 2 drzwi to mediana mocy silnika wynosi: 170, jesli samochod posiada 4 drzwi to mediana mocy silnika wynosi: 200



# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data%>%
  mutate(city.km= 235.21/city.mpg) %>% 
  mutate(.by=c('Year','Make'),Mean_city_km=mean(city.km,na.rm=TRUE))%>% 
  distinct(Mean_city_km,Year,Make)%>%select(Mean_city_km,Year,Make) %>%top_n(1,Mean_city_km)

# Odp.: Samochody marki Bugatti z lat 2008, 2009 mają największe średnie spalanie w mieście, które wynosi 29.40125 L/100km. 


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?

data %>%
  filter(Year >= 2007 & Year<=2017) %>% 
  select(Year,Vehicle.Style,Popularity) %>%
  mutate(.by=c('Year','Vehicle.Style'),MEAN_POP=mean(Popularity,na.rm=TRUE))%>%
  mutate(.by=Year,MAX_POP=max(MEAN_POP,na.rm=TRUE))%>%
  filter(MEAN_POP==MAX_POP)%>%distinct(Year,Vehicle.Style,MAX_POP)%>%arrange(Year)

data %>%
  filter(Year >= 2007 & Year<=2017)%>% 
  select(Year,Vehicle.Style,Popularity) %>%
  mutate(.by=c('Year','Vehicle.Style'),MEAN_POP=mean(Popularity,na.rm=TRUE))%>%
  mutate(.by=Year,MAX_POP=max(MEAN_POP,na.rm=TRUE))%>%
  filter(MEAN_POP==MAX_POP)%>%distinct(Year,Vehicle.Style,MAX_POP)%>%arrange(Year)%>%group_by(Vehicle.Style)%>%summarise(n=n())%>%top_n(1,n)
  
# Odp.:W tym okresie wystąpiły najwięcej razy Passenger Van, Cargo Minivan, Cargo Van
# Year       Vehicle.Style  MAX_POP
# 2007       Cargo Minivan 2964.333
# 2008     Crew Cab Pickup 2461.217
# 2009 Extended Cab Pickup 3583.375
# 2010 Extended Cab Pickup 3093.000
# 2011  Regular Cab Pickup 5657.000
# 2012           Cargo Van 5657.000
# 2012       Passenger Van 5657.000
# 2013           Cargo Van 5657.000
# 2013       Passenger Van 5657.000
# 2014           Cargo Van 2857.571
# 2015       Cargo Minivan 4337.000
# 2016       Cargo Minivan 4050.846
# 2017       Passenger Van 5657.000


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data%>%
  select(Year,Make,MSRP,Market.Category) %>%
  filter(Year>=2000 & Market.Category=="Luxury,Performance") %>%
  mutate(.by=Make,MEDIAN=median(MSRP,na.rm=TRUE))%>%
  distinct(Make,MEDIAN)%>%top_n(1,MEDIAN)
data%>%
  select(Year,Make,MSRP,Market.Category,Engine.Cylinders,Engine.HP) %>%
  filter(Year>=2000 & Market.Category=="Luxury,Performance") %>%
  mutate(.by=Make,MEDIAN=median(MSRP,na.rm=TRUE))%>%
  mutate(.by=Make,MEAN_CYLINDERS=mean(Engine.Cylinders,na.rm=TRUE))%>%
  mutate(.by=Make,MEAN_HP=mean(Engine.HP,na.rm=TRUE))%>%
  distinct(Make,MEDIAN,MEAN_CYLINDERS,MEAN_HP)%>%
  arrange(desc(MEDIAN))
odp = c(10.666667-6.000000,397.0000-311.0000)
# Odp.:Volkswagen, a różnica średniej liczby cylindrów wynosi: 4.666667, a różnica średniej mocy silnika: 86


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?
data%>%
  filter(Transmission.Type=="AUTOMATIC",Vehicle.Size=="Midsize")%>%summarise(Q1=quantile(city.mpg,0.1,na.rm=TRUE),Q9=quantile(city.mpg,0.9,na.rm=TRUE))

# Odp.: 0.1 kwantyl: 14 ,0.9 kwantyl: 25.
 


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?
data%>%
  filter(Make=="Porsche" & Engine.HP<=300)%>%
  top_n(2,Popularity)%>%select(Year,Model,Popularity)


data%>%
  filter(Make=="Porsche" & Engine.HP<=300)%>%select(Year,Model,Popularity)%>%arrange(Year)%>%group_by(Model)%>%summarise(n=n())%>%top_n(2,n)

# Odp.: Nie grupując na lata wychodzi na to, że wszystkie modele sa równie popularne. 
# Grupując po latach, zliczając ilosc wystąpień w kazdym roku otrzymamy, że drugi najbardziej popularny był 944 oraz Cayenne.  


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100km?
  
data %>% 
  select(Engine.Fuel.Type,Driven_Wheels,Engine.HP,highway.MPG)%>%
  mutate(.by=c('Engine.Fuel.Type','Driven_Wheels'),MED_HP=median(Engine.HP,na.rm=TRUE))%>%
  top_n(1,MED_HP)%>%
  mutate(HIGHWAY_KM=235.21/highway.MPG)%>%
  mutate(.by=c('Engine.Fuel.Type','Driven_Wheels'),MEAN_KM=mean(HIGHWAY_KM,na.rm=TRUE))%>%
  distinct(Engine.Fuel.Type,Driven_Wheels,MEAN_KM)

# Odp.:Dla kombinacji: flex-fuel (premium unleaded required/E85) - all wheel drive, mediana koni mechanicznych jest największa. Srednie spalanie na autostradzie dla tej kombinacji wynosi: 12.26227


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100km? Ile wynosi ta różnica?

data%>%
  select(Make,Model,city.mpg,highway.MPG)%>%
  mutate(diff_km=abs(235.21*((1/city.mpg)-(1/highway.MPG))))%>%
  mutate(.by=c('Make','Model'),Mean_diff=mean(diff_km,na.rm=TRUE)) %>%
  top_n(1,Mean_diff)%>%select(Make,Model,Mean_diff)
  
# Odp.: Kombinacja: Ferrari - Enzo, Róznica wynosi: 14.0006 L/100km. 

