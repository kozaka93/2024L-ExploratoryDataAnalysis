library(dplyr)

data <- read.csv("data.csv")
head(data)

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

data%>%
  group_by(Transmission.Type)%>%
  summarise(mean_mocy=median(Engine.HP, na.rm = TRUE))


# Odp.:
#1 AUTOMATED_MANUAL        220
#2 AUTOMATIC               253
#3 DIRECT_DRIVE            147
#4 MANUAL                  172
#5 UNKNOWN                 125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?
data%>%
  filter(Make=='Mercedes-Benz',Engine.Fuel.Type=='diesel')%>%
  summarise(rozstep=IQR(city.mpg))
  


# Odp.:#7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data%>%
  filter(Engine.Cylinders==4)%>%
  group_by(Make)%>%
  summarise(detal=mean(MSRP))%>%
  arrange(-detal)


# Odp.:  Alfa Romeo    61600 


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
# ilości aut według rodzajów napędu

data%>%
  filter(Make=='BMW')%>%
  group_by(Driven_Wheels)%>%
  summarise(ilosc=n())%>%
  arrange(-ilosc)%>%
  head(1)

#model

data%>%
  filter(Make=='BMW', Driven_Wheels=='rear wheel drive')%>%
  group_by(Model)%>%
  summarise(ilosc=n())%>%
  arrange(-ilosc)%>%
  head(1)
  
  
# Odp.: 189 rear wheel drive, model 1 Series


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data%>%
  filter(Make=='Volkswagen')%>%
  group_by(Number.of.Doors)%>%
  summarise(mediana=median(Engine.HP))

# Odp.: ilość drzwi - 2  mediana- 170, ilość drzwi 4 mediana- 200
# wraz ze wrostem ilości drzwi, wzrasta moc silnika


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data%>%
  group_by(Make,Year)%>%
  summarise(srednia = mean(city.mpg, na.rm=T))%>%
  mutate(spalanie=235.21/srednia)%>%
  arrange(-spalanie)%>%
  head(1)

# Odp.:   Make     Year srednia spalanie (w L/100km, stąd 235,21 jest podzielone przez średnią)
#        Bugatti  2008       8     29.4


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?

#znalezienie najpopularniejszych modeli

data%>%
  filter(2007<= Year, Year<=2017)%>%
  group_by(Year, Vehicle.Style)%>%
  summarise(srednia=mean(Popularity))%>%
  top_n(1,srednia) -> popularnosc_latami

#zliczenie wystąpień modeli latami

 popularnosc_latami%>%
   group_by(Vehicle.Style)%>%
   summarise(ilosc=n())%>%
   arrange(-ilosc)
  
  

# Odp.: najpopularniejsze modele w każdym roku
#    Year  Vehicle.Style        srednia
# 1  2007 Cargo Minivan         2964.
# 2  2008 Crew Cab Pickup       2461.
# 3  2009 Extended Cab Pickup   3583.
# 4  2010 Extended Cab Pickup   3093 
# 5  2011 Regular Cab Pickup    5657 
# 6  2012 Cargo Van             5657 
# 7  2012 Passenger Van         5657 
# 8  2013 Cargo Van             5657 
# 9  2013 Passenger Van         5657 
# 10  2014 Cargo Van            2858.
# 11  2015 Cargo Minivan        4337 
# 12  2016 Cargo Minivan        4051.
# 13  2017 Passenger Van        5657 
 
 
 #najczęściej występujące modele z powyższej listy
 # Vehicle.Style       ilosc
 # 1 Cargo Minivan           3
 # 2 Cargo Van               3
 # 3 Passenger Van           3


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data%>%
  filter(Market.Category=='Luxury,Performance' & Year>=2000)%>%
  select(MSRP,Engine.Cylinders,Engine.HP, Make)%>%
  group_by(Make)%>%
  summarise(cena=median(MSRP),cylindry=mean(Engine.Cylinders), moc=mean(Engine.HP))%>%
  arrange(-cena)%>%
   slice(1,n())
  


# Odp.: 
 # Make         cena    cylindry moc
 # 1 Volkswagen 94600     10.7   397
 # 2 Hyundai    39625      6     311
 
#najwyższa mediana sugerowanej ceny - Volkswagen , najniższa - Hyundai
#różnica między średnią liczbą cylindrów - 4.7
#różnica między mocami silników - 86
#Volkswagen        10.7   397
#Hyundai            6     311 


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data%>%
  filter(Vehicle.Size=='Midsize', Transmission.Type=='AUTOMATIC')%>%
  summarise(
    kwantyl_01 = quantile(city.mpg,0.1),
    kwantyl_09 = quantile(city.mpg,0.9))



# Odp.: kwantyl 0.1 - 14, kwantyl 0.9 - 25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data%>%
  filter(Make=='Porsche', Engine.HP <= 300)%>%
  arrange(-Popularity)%>%
  head(2)


# Odp.:  Porsche        944


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data%>%
  group_by(Engine.Fuel.Type, Driven_Wheels)%>%
  summarise(mediana=median(Engine.HP), spalanie = 235.21/mean(highway.MPG))%>%
  arrange(-mediana)%>%
  head(1)



# Odp.:  Engine.Fuel.Type                          Driven_Wheels   mediana spalanie(średnie)
#      flex-fuel (premium unleaded required/E85) all wheel drive     608     12.2


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data%>%
  group_by(Make,Model)%>%
  summarise(roznica = mean(abs(235.21/highway.MPG - 235.21/city.mpg)))%>%
  arrange(-roznica)%>%
  head(1)

# Odp.:    Make    Model roznica
#         Ferrari Enzo     14.0
 
