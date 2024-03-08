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
  group_by(Transmission.Type) %>% 
  summarise(mediana=median(Engine.HP, na.rm=TRUE))


# Odp.:
# AUTOMATED_MANUAL 220
# AUTOMATIC        253
# DIRECT_DRIVE     147
# MANUAL           172
# UNKNOWN          125



# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?
 data %>% 
  filter(Make=="Mercedes-Benz" & Engine.Fuel.Type=="diesel") %>% 
  summarise(rozstep=IQR(city.mpg)) 
  
# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders==4) %>% 
  group_by(Make) %>% 
  summarise(srednia=mean(MSRP)) %>% 
  arrange(-srednia) %>% 
  head(1)

# Odp.: Alfa Romeo   61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>% 
  filter(Make=="BMW") %>% 
  group_by(Driven_Wheels) %>% 
  summarise(n=n())
  
data %>% 
  filter(Make=="BMW" & Driven_Wheels=="rear wheel drive") %>% 
  group_by(Model) %>% 
  summarise(n=n()) %>% 
  arrange(-n) %>% 
  head(1)
  
# Odp.: 1) Więcej jest aut z napędem na tył (rear wheel drive)
#2) Najczęściej pojawia się 1 Series


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make=="Volkswagen" & (Number.of.Doors==2 | Number.of.Doors==4)) %>% 
  group_by(Number.of.Doors) %>% 
  summarise(mediana=median(Engine.HP))


# Odp.: Dla samochodów 2-drzwiowych mediana wynosi 170, a dla 4-drzwiowych
# 200, więc mediana wzrasta o 30


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>% 
  mutate(L_100km=235.215/city.mpg) %>% 
  group_by(Year, Make) %>% 
  summarise(srednia=mean(L_100km)) %>% 
  arrange(-srednia) %>%
  head(1)


# Odp.: 2008 Bugatti 29.4 L/100km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
data %>% 
  filter(Year>=2007 & Year<=2017) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(srednia=mean(Popularity)) %>% 
  filter(srednia==max(srednia))

data %>% 
  filter(Year>=2007 & Year<=2017) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(srednia=mean(Popularity)) %>% 
  filter(srednia==max(srednia)) %>% 
  group_by(Vehicle.Style) %>% 
  summarise(n=n()) %>% 
  arrange(-n)

# Odp.: 1)
#1   2007 Cargo Minivan         2964.
#2   2008 Crew Cab Pickup       2461.
#3   2009 Extended Cab Pickup   3583.
#4   2010 Extended Cab Pickup   3093 
#5   2011 Regular Cab Pickup    5657 
#6   2012 Cargo Van             5657 
#7   2012 Passenger Van         5657 
#8   2013 Cargo Van             5657 
#9   2013 Passenger Van         5657 
#10  2014 Cargo Van             2858.
#11  2015 Cargo Minivan         4337 
#12  2016 Cargo Minivan         4051.
#13  2017 Passenger Van         5657 

#2)
#Najczęściej wystąpiły po równo trzy style:
 #Cargo Minivan
 #Cargo Van 
 #Passenger Van  


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
  filter(Market.Category=="Luxury,Performance" & Year>=2000) %>% 
  group_by(Make) %>% 
  summarise(mediana=median(MSRP)) %>% 
  arrange(-mediana) %>% 
  head(1)

#najniższą medianę ma Hyundai
data %>% 
  filter(Market.Category=="Luxury,Performance" & Year>=2000 & (Make=="Hyundai" | Make=="Volkswagen")) %>% 
  group_by(Make) %>% 
  summarise(sr_cyl=mean(Engine.Cylinders), sr_moc=mean(Engine.HP))


# Odp.: 1) Volkswagen 94600
# 2) Hyundai ma średnio 6 cylindrów, a Volkswagen 10.7, czyli o 4.7 więcej. Natomaiast
#Hyundai ma średnią moc równą 311 koni, a Volkswagen 397 koni, czyli o 86 więcej


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>% 
  filter(Transmission.Type=="AUTOMATIC" & Vehicle.Size=="Midsize") %>% 
  summarise(Q01=quantile(city.mpg, 0.1), Q09=quantile(city.mpg, 0.9)) 


# Odp.: 0.1 kwantyl wynosi 14 mili na galon, 0.9 kwantyl 25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>% 
  filter(Engine.HP<=300 & Make=="Porsche") %>% 
  arrange(-Popularity) %>% 
  select(Model, Popularity)

# Odp.: Wszystkie modele Porsche o mocy silnika <=300 koni mają tą samą popularnośc,
#równą 1715


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  summarise(mediana=median(Engine.HP, na.rm=TRUE)) %>% 
  arrange(-mediana) %>% 
  head(1)

data %>% 
  filter(Engine.Fuel.Type=="flex-fuel (premium unleaded required/E85)" & Driven_Wheels=="all wheel drive") %>% 
  mutate(L_100=235.215/highway.MPG) %>% 
  summarise(srednie=mean(L_100))

# Odp.: 1) Dla flex-fuel (premium unleaded required/E85) z napędem all wheel drive i
#wynosi 608 mili na galon
#2) 12.26253 L/100km


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>% 
  mutate(L_100_miasto=235.215/city.mpg, L_100_autostrada=235.215/highway.MPG) %>% 
  mutate(roznica=abs(L_100_miasto-L_100_autostrada)) %>% 
  group_by(Make, Model) %>% 
  summarise(srednia=mean(roznica)) %>% 
  arrange(-srednia) %>% 
  head(1)


# Odp.: Ferrari Enzo 14.0 L/100km

