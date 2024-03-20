library(dplyr)

data <- read.csv("data.csv")
View(data)

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
  group_by(Transmission.Type) %>% 
  summarise(median_Engine.HP = median(Engine.HP,na.rm = TRUE))


# Odp.: 
#  Transmission.Type        median_Engine.HP
# 1 AUTOMATED_MANUAL             220
# 2 AUTOMATIC                    253
# 3 DIRECT_DRIVE                 147
# 4 MANUAL                       172
# 5 UNKNOWN                      125



# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>% 
  filter(Make=="Mercedes-Benz", Engine.Fuel.Type=="diesel") %>% 
  summarise(IQR=IQR(city.mpg, na.rm=TRUE))
  


# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders==4) %>% 
  group_by(Make) %>% 
  summarise(mean_price= mean(MSRP)) %>% 
  arrange(-mean_price) %>% 
  head(1)


# Odp.: Alfa Romeo, 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>% 
  filter(Make=="BMW") %>% 
  group_by(Driven_Wheels) %>% 
  summarise(n = n()) %>% 
  top_n(1,n)

data %>% 
  filter(Driven_Wheels=="rear wheel drive") %>% 
  group_by(Model) %>% 
  summarise(n=n()) %>% 
  top_n(1,n)



# Odp.: Na tył, Silverado 1500 


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make=="Volkswagen") %>% 
  group_by(Number.of.Doors) %>% 
  summarise(median_Engine.HP = median(Engine.HP))

# Odp.: 
#          Number.of.Doors median_Engine.HP
# 1               2              170
# 2               3               90
# 3               4              200


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>% 
  mutate(mean_city.mpg=mean(city.mpg)*0.425144) %>% 
  arrange(-mean_city.mpg) %>% 
  head(1) %>% 
  select(Make, Year, mean_city.mpg)


# Odp.: BMW 2011, spalanie wynosi: 8.389475


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
data %>%
  filter(Year<2018 & Year>2006) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(mean_popularity=mean(Popularity)) %>%
  arrange(Year, -mean_popularity) %>% 
  top_n(1,mean_popularity) %>% 
  select(Year, Vehicle.Style)

data %>%
  filter(Year<2018 & Year>2006) %>% 
  group_by(Vehicle.Style) %>% 
  summarise(mean_popularity=mean(Popularity)) %>%
  top_n(1,mean_popularity)

# # Odp.: Przez 10 lat: Cargo Minivan    
#    Year Vehicle.Style      
# 1  2007 Cargo Minivan      
# 2  2008 Crew Cab Pickup    
# 3  2009 Extended Cab Pickup
# 4  2010 Extended Cab Pickup
# 5  2011 Regular Cab Pickup 
# 6  2012 Cargo Van          
# 7  2012 Passenger Van      
# 8  2013 Cargo Van          
# 9  2013 Passenger Van      
# 10  2014 Cargo Van          
# 11  2015 Cargo Minivan      
# 12  2016 Cargo Minivan      
# 13  2017 Passenger Van  


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
  filter(Market.Category=="Luxury,Performance",Year>=2000) %>% 
  group_by(Make) %>% 
  summarise(median_price=median(MSRP)) %>% 
  top_n(1,median_price) %>% 
  select(Make)

data %>% 
  filter(Market.Category=="Luxury,Performance",Year>=2000) %>% 
  group_by(Make) %>% 
  summarise(median_price=median(MSRP)) %>% 
  top_n(-1,median_price) %>% 
  select(Make)

data %>% 
  filter(Make=="Volkswagen" | Make=="Hyundai") %>% 
  group_by(Make) %>% 
  summarise(mean=mean(Engine.Cylinders, na.rm=TRUE))

data %>% 
  filter(Make=="Volkswagen" | Make=="Hyundai") %>% 
  group_by(Make) %>% 
  summarise(mean=mean(Engine.HP, na.rm=TRUE))

# Odp.: Volkswagen, różnica średniej liczby cylindrów = 0.30, różnica mocy = 12 


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>% 
  filter(Vehicle.Size=="Midsize", Transmission.Type=="AUTOMATIC") %>% 
  summarise(value=quantile(city.mpg,1/10))

data %>% 
  filter(Vehicle.Size=="Midsize", Transmission.Type=="AUTOMATIC") %>% 
  summarise(value=quantile(city.mpg,9/10))

# Odp.: 0.1 kwartyl = 14, 0.9 kwartyl = 25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?
data %>% 
  filter(Make=="Porsche", Engine.HP<=300) %>% 
  group_by(Model,Popularity) %>% 
  summarise(sum=sum(Popularity)) %>% 
  arrange(-sum)

# Odp.: Są dwa: 944 oraz Cayenne


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?

data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  summarise(median=median(Engine.HP,na.rm=TRUE)) %>% 
  arrange(-median)

data %>% 
  filter(Engine.Fuel.Type=="flex-fuel (premium unleaded required/E85)", Driven_Wheels=="all wheel drive") %>% 
  summarise(mean=mean(highway.MPG,na.rm=TRUE)*0.425144)


# Odp.: flex-fuel (premium unleaded required/E85) + all wheel drive, 8.184022


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>% 
  group_by(Make,Model) %>% 
  mutate(diff=abs(highway.MPG-city.mpg)*0.425144) %>% 
  summarise(mean=mean(diff)) %>% 
  arrange(-mean)


# Odp.: Kia        Soul EV   11.9 

