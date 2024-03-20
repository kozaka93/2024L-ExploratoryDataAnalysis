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
  summarise(mediana = median(Engine.HP, na.rm = TRUE))



# Odp.: Mediana(liczba koni mechanicznych)
# 1 AUTOMATED_MANUAL      220
# 2 AUTOMATIC             253
# 3 DIRECT_DRIVE          147
# 4 MANUAL                172
# 5 UNKNOWN               125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>%
  filter(Make == "Mercedes-Benz", Engine.Fuel.Type == "diesel") %>%
  summarise(IQR = quantile(city.mpg, na.rm = TRUE)[4] - quantile(city.mpg, na.rm = TRUE)[2])

# Odp.: IQR = 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>%
  filter(Engine.Cylinders == 4) %>%
  group_by(Make) %>%
  summarise(cena = mean(MSRP)) %>%
  arrange(-cena) %>%
  head(1)



# Odp.: Najyższą cenę detaliczną sugeruje wówczas Alfa Romeo i wynosi ona 61600 (nie wiadomo jaka jednostka).


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.

data %>%
  filter(Make == "BMW") %>%
  group_by(Driven_Wheels) %>%
  summarise(n = n()) %>%
  arrange(-n)
  
data %>%
  filter(Make == "BMW", Driven_Wheels == "rear wheel drive") %>%
  group_by(Model) %>%
  summarise(n= n()) %>%
  arrange(-n) %>%
  head(1)
  


# Odp.: Najwięcej jest BMW z napędem na tył, potem na 4 koła i najmniej na przód.
#       Najczesciej wystepujacy model dla napedu na tył to 1 Series.


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>%
  filter(Make == "Volkswagen") %>%
  group_by(Number.of.Doors) %>%
  summarise(mediana = median(Engine.HP))



# Odp.: Dla samochodów z 4 drzwiami mediana mocy silnika wynosi 200, a dla tych z 2 drzwiami 170.
#       Zartem dla 4-drzwiowych mediana mocy silnika jest o 30 wyższa niż dla 2-drzwiowych.


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>%
  group_by(Make, Year) %>%
  summarise(spalanie = mean(235/city.mpg)) %>%
  ungroup() %>%
  top_n(1, spalanie)
  

# Odp.: Bugatti z lat 2008 i 2009 spalają najwięcej, bo średnio 29.4 L/100km w mieście.


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?

data %>%
  filter(Year <= 2017 & Year >= 2007) %>%
  group_by(Year, Vehicle.Style) %>%
  summarise(p = mean(Popularity)) %>%
  filter(p == max(p)) %>%
  select(Year, Vehicle.Style) -> data1
  

data1 %>%
  group_by(Vehicle.Style) %>%
  summarise(n = n()) %>%
  top_n(1, n)

# Odp.: Najpopularniejsze style w latach 2007-2017:
#  2007 Cargo Minivan       2964.
#  2008 Crew Cab Pickup     2461.
#  2009 Extended Cab Pickup 3583.
#  2010 Extended Cab Pickup 3093 
#  2011 Regular Cab Pickup  5657 
#  2012 Cargo Van           5657 
#  2012 Passenger Van       5657 
#  2013 Cargo Van           5657 
#  2013 Passenger Van       5657 
#  2014 Cargo Van           2858.
#  2015 Cargo Minivan       4337 
#  2016 Cargo Minivan       4051.
#  2017 Passenger Van       5657 
#Najczesciej popularny w tych latach byly Cargo Minivan, Cargo Van i Passenger Van.



# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>%
  filter(Year >= 2000 & Market.Category == "Luxury,Performance") %>%
  group_by(Make) %>%
  summarise(mediana = median(MSRP, na.rm = TRUE)) %>%
  arrange(-mediana) -> data2

head(data2,1)
tail(data2,1)

data %>%
  filter(Year >= 2000 & Market.Category == "Luxury,Performance") %>%
  group_by(Make) %>%
  summarise(cylindry = mean(Engine.Cylinders, na.rm = TRUE), silnik = mean(Engine.HP, na.rm =TRUE)) %>%
  filter(Make == "Volkswagen" | Make == "Hyundai")


# Odp.: Volkswagen ma najwyższą medianę sugerowanej ceny. Różnica w liczbie cylindrów: Hyundai ma 6, a 
# Volkswagen 10.7, natomiast w mocy silnika: Hyundai 311, a Volkswagen 397.


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>%
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>%
  summarise(kwantyl_01 = quantile(city.mpg, 0.1), kwantyl_09 = quantile(city.mpg, 0.9))


# Odp.: 0.1 kwantyl wynosi 14 mpg, a 0.9 kwantyl to 25 mpg.


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>%
  filter(Engine.HP <= 300, Make == "Porsche") %>%
  group_by(Model) %>%
  summarise(popul = mean(Popularity)) %>%
  arrange(-popul)

data %>%
  filter(Engine.HP <= 300, Make == "Porsche") %>%
  group_by(Model) %>%
  summarise(popul = sum(Popularity)) %>%
  arrange(-popul)

# Odp.: Jeśli popularność modelu liczymy poprzez średnią popularności każdego pojazdu to nie ma
# drugiego najbardziej popularnego modelu Porsche, bo wszystkie mają średnią popularność taką samą = 1715,
# ale jeśli policzymy sumując popularności poszczególnych pojazdów to będzie to 944 i Cayenne
# (myślę jednak, że pomysł ze średnią jest bardziej odpowiedni)



# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>%
  group_by(Engine.Fuel.Type, Driven_Wheels) %>%
  summarise(mediana = median(Engine.HP)) %>%
  arrange(-mediana) %>%
  head(1)

data %>%
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)", Driven_Wheels == "all wheel drive") %>%
  group_by(Engine.Fuel.Type, Driven_Wheels) %>%
  summarise(spalanie = mean(235/highway.MPG, na.rm = TRUE))


# Odp.: Mediana jest największa dla paliwa typu flex-fuel (premium unleaded required/E85) 
# i napędu na 4 koła. A średnie spalanie dla tej kombinacji na autostradzie wynosi 12.3 L/100km.


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>%
  group_by(Make, Model) %>%
  summarise(miasto = mean(235/city.mpg), autostrada = mean(235/highway.MPG)) %>%
  mutate(roznica = abs(miasto - autostrada)) %>%
  arrange(-roznica)


# Odp.: Największa różnica w średnim spalaniu w mieście i na autostradzie jest dla Ferrari Enzo
# i wynosi 14 L/100km.
