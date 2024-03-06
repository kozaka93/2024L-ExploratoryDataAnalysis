library(dplyr)

data <- read.csv("data.csv")

names(data)
# Zadanie 0 ---------------------------------------------------------------
# Jaka marka samochodu najczesciej wystepuje w zbiorze danych?

data %>%
  group_by(Make) %>%
  summarise(n = n()) %>%
  arrange(-n) %>% 
  select(Make) %>% 
  head(1)

# Odp.: Chevrolet   


# Zadanie 1 ---------------------------------------------------------------
# Jaka jest mediana mocy silnika dla samochodów w zależności od rodzaju
# skrzyni biegów?

data %>% 
  group_by(Transmission.Type) %>% 
  summarise(mediana = median(Engine.HP, na.rm = T)) %>% 
  rename(typ = "Transmission.Type")
  

# Odp.:
# 1 AUTOMATED_MANUAL     220
# 2 AUTOMATIC            253
# 3 DIRECT_DRIVE         147
# 4 MANUAL               172
# 5 UNKNOWN              125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

  data %>% 
    filter(Make == "Mercedes-Benz" & Engine.Fuel.Type == "diesel") %>% 
    summarise(IQR = 
                quantile(city.mpg, probs = 0.75) - quantile(city.mpg, probs = 0.25))

# Odp.: 7.25 daje program, 

#wlasne obliczenia daja 6.5, obie odpowiedzi sa poprawne, bo  liczylem kwantyle jako średnia arytmetyczna, a R liczy jakos inaczej



# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders == 4) %>% 
  group_by(Make) %>% 
  summarise(srednia = mean(MSRP, na.rm = T)) %>% 
  arrange(-srednia) %>% 
  head(1)

# Odp.: Alfa Romeo   61600



# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
# a)
data %>% 
  filter(Make == "BMW") %>% 
  group_by(Driven_Wheels) %>% 
  summarise(licznik = n()) %>% 
  arrange(-licznik)%>% 
  head(1)

#b)

data %>% 
  filter(Make == "BMW", Driven_Wheels == "rear wheel drive") %>% 
  group_by(Model) %>% 
  summarise(licznik = n()) %>% 
  arrange(-licznik) %>% 
  head(1)

# Odp.: rear wheel drive, BMW 1 Series, 16 modeli



# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Number.of.Doors == c(2,4) & Make == "Volkswagen") %>% 
  group_by(Number.of.Doors) %>% 
  summarise(mediana = median(Engine.HP, na.rm = T))


# Odp.:
# liczba drzwi   mediana mocy silnika
#      2        170.
#      4        200.
# wiecej drzwi => wiecej mocy


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.


# z internetu: 1 US gallon per mile = 235.214583 liters per 100km
# powiedzmy wiec ze okolo 235.21

mnoznik <- 235.21

# 100km = 235.21/x litrow, gdzie x to liczba mil na galon

data %>% 
  group_by(Make, Year) %>% 
  summarise(srednia = mean(city.mpg, na.rm=T)) %>% 
  mutate(spalanie = mnoznik/srednia) %>% 
  arrange(-spalanie) %>% 
  head(1)

# Odp.: Bugatti, 2008r, spalanie 29.4l/100km w miescie



# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?


#a)
a <- data %>% 
  filter(Year >= 2007 , Year <= 2017) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(popularnosc = mean(Popularity)) %>% 
  top_n(1, popularnosc)

#print(a)

#b)
a %>% 
  group_by(Vehicle.Style) %>% 
  summarise(licznik = n()) %>% 
  top_n(1,licznik) %>% 
  select(Vehicle.Style, licznik)


# Odp.:
#a)
#  2007 Cargo Minivan             2964.
#  2008 Crew Cab Pickup           2461.
#  2009 Extended Cab Pickup       3583.
#  2010 Extended Cab Pickup       3093 
#  2011 Regular Cab Pickup        5657 
#  2012 Cargo Van                 5657 
#  2012 Passenger Van             5657 
#  2013 Cargo Van                 5657 
#  2013 Passenger Van             5657 
#  2014 Cargo Van                 2858.
#  2015 Cargo Minivan             4337 
#  2016 Cargo Minivan             4051.
#  2017 Passenger Van             5657 

#b)
#Vehicle.Style licznik
# Cargo Minivan       3
# Cargo Van           3
# Passenger Van       3



# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok?
#Jak różni się średnia liczba cylindrów i mocy silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
  filter(Year >= 2000, Market.Category == "Luxury,Performance") %>% 
  group_by(Make) %>% 
  summarise(med_Cena = median(MSRP),
            avg_Cylindry = mean(Engine.Cylinders),
            avg_MocSilnika = mean(Engine.HP)) %>% 
  arrange(-med_Cena) %>% 
  select(Make, med_Cena, avg_Cylindry, avg_MocSilnika) %>% 
  slice(c(1,n()))

           

# Odp.:
#a) Volkswagen Auto
#b) usredniajac:
#Make         | mediana ceny | avg cylindry_silnika  | avg moc_silnika
# Volkswagen  |   94600       |       10.7           |       397
# Hyundai     |   39625       |       6              |       311


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>%
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>% 
  summarise(kwantyl_zerojeden = quantile(city.mpg, probs = 0.1),
         kwantyl_zerodziewiec = quantile(city.mpg, probs = 0.9)) 

# Odp:
#   kwantyl 0.1   0.9
#           14    25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>% 
  filter(Make == "Porsche", Engine.HP <= 300) %>%
  group_by(Model)%>% 
  summarise(popularnosc = sum(Popularity)) %>% 
  arrange(-popularnosc)%>% 
  slice(2) %>% 
  select(Model)

# Odp.: model: Porsche 944


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?

data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  mutate(mediana_HP = median(Engine.HP, na.rm = T),
         srednie_spalanie = mnoznik/mean(highway.MPG, na.rm = T))%>% 
  select(Engine.Fuel.Type, Driven_Wheels, mediana_HP, srednie_spalanie) %>% 
  arrange(-mediana_HP) %>% 
  head(1)

# Odp.:
# a)
# Engine.Fuel.Type                          Driven_Wheels     mediana_HP      srednie spalanie
# flex-fuel (premium unleaded required/E85) all wheel drive        608             12.2
#
#   srednie_spalanie (tak dokladniej) wychodzi
#      12.2187


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

mnoznik <- 235.21

data %>% 
  mutate(spalanie_miasto_L_km_ = mnoznik/city.mpg,
          spalanie_autostrada_L_km = mnoznik/highway.MPG)%>% 
  group_by(Make, Model) %>%
  mutate(spalanie_roznica = mean(
    abs(spalanie_miasto_L_km_ - spalanie_autostrada_L_km))) %>% 
  arrange(-spalanie_roznica) %>% 
  select(Make, Model, spalanie_roznica) %>% 
  head(1)

# Make      Model   spalanie_roznica
# Ferrari   Enzo              14.0
 
