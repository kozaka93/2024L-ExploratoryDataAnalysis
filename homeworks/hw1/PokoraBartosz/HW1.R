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
  summarise(median(Engine.HP, na.rm = TRUE))

# Odp.:
#   Transmission.Type       median(Engine.HP)
# 1 AUTOMATED_MANUAL        220
# 2 AUTOMATIC               253
# 3 DIRECT_DRIVE            147
# 4 MANUAL                  172
# 5 UNKNOWN                 125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

# Dla city.mpg
data %>%
  filter(Make == "Mercedes-Benz", Engine.Fuel.Type == "diesel") %>%
  summarise(IQR(city.mpg))

# Dla spalania L/100km
data %>%
  filter(Make == "Mercedes-Benz", Engine.Fuel.Type == "diesel") %>%
  mutate(city.Lp100km = 235.215/city.mpg) %>%
  summarise(IQR(city.Lp100km))

# Odp.: 3.395831 (dla spalania L/100km)     //  7.25 (dla mpg)


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>%
  filter(Engine.Cylinders == 4) %>%
  group_by(Make) %>%
  summarise(MEAN.MSRP = mean(MSRP)) %>%
  arrange(-MEAN.MSRP)

# Odp.: Alfa Romeo z ceną 61600.


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
  summarise(n = n()) %>%
  arrange(-n)

# Odp.: Więcej jest z napędem na tył. Najczęściej występujący model to 1 Series.


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>%
  filter(Make == "Volkswagen") %>%
  group_by(Number.of.Doors) %>%
  summarise(median(Engine.HP)) %>%
  filter(Number.of.Doors %in% c(2, 4))

# Odp.:
#     Number.of.Doors `median(Engine.HP)`
# 1   2                 170
# 2   4                 200


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>%
  group_by(Make, Year) %>%
  summarise(mean.city.mpg = mean(city.mpg)) %>%
  arrange(mean.city.mpg) %>%
  head() %>%
  mutate(mean.city.Lp100km = 235.215/mean.city.mpg)

# Odp.: Bugatti z lat 2008 i 2009 (remis). Spalanie wynosi 29.4 L/100km.


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
data2 <- data %>%
  filter(Year %in% 2007:2017) %>%
  group_by(Year, Vehicle.Style) %>%
  summarise(Popularity = mean(Popularity)) %>%
  group_by(Year) %>%
  filter(Popularity == max(Popularity))
data2

data2 %>%
  group_by(Vehicle.Style) %>%
  summarise(n = n()) %>%
  arrange(-n)

# Odp.: Najczęściej (po 3 razy) wystąpiły modele:
# Cargo Minivan, Cargo Van, Passenger Van.

#     Year Vehicle.Style       Popularity
# 1   2007 Cargo Minivan            2964.
# 2   2008 Crew Cab Pickup          2461.
# 3   2009 Extended Cab Pickup      3583.
# 4   2010 Extended Cab Pickup      3093 
# 5   2011 Regular Cab Pickup       5657 
# 6   2012 Cargo Van                5657 
# 7   2012 Passenger Van            5657 
# 8   2013 Cargo Van                5657 
# 9   2013 Passenger Van            5657 
# 10  2014 Cargo Van                2858.
# 11  2015 Cargo Minivan            4337 
# 12  2016 Cargo Minivan            4051.
# 13  2017 Passenger Van            5657 


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>%
  filter(Market.Category == "Luxury,Performance", Year >= 2000) %>%
  group_by(Make) %>%
  summarise(median.MSRP = median(MSRP)) %>%
  arrange(-median.MSRP)

data3 <- data %>%
  filter(Make %in% c("Volkswagen", "Hyundai")) %>%
  group_by(Make) %>%
  summarise(mean.Engine.Cylinders = mean(Engine.Cylinders, na.rm = TRUE), mean.Engine.HP = mean(Engine.HP, na.rm = TRUE))
data3

rbind(data3, c("difference", data3$mean.Engine.Cylinders[1] - data3$mean.Engine.Cylinders[2],
               data3$mean.Engine.HP[1] - data3$mean.Engine.HP[2]))

# Odp.: Volkswagen ma najwyższą medianę sugerowanej ceny. (a Hyundai najniższą)
# Różnica w średniej liczbie cylindrów wynosi 0.3, a w średniej mocy 12.15. (oba na korzyść Hyundai)


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>%
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>%
  summarise(quantile(235.215/city.mpg, c(0.1, 0.9)))

# Odp.:   0.1 kwantyl:    9.4
#         0.9 kwantyl:    16.8
# (dla spalania w L/100km)


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>%
  filter(Make == "Porsche", Engine.HP <= 300) %>%
  group_by(Model) %>%
  summarise(Popularity = mean(Popularity)) %>%
  arrange(-Popularity)

# Odp.: Wszystkie takie modele są tak samo popularne,
# więc nie ma drugiego najbardziej popularnego modelu.


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>%
  group_by(Engine.Fuel.Type, Driven_Wheels) %>%
  summarise(median.Enginge.HP = median(Engine.HP), mean.highway.Lp100km = mean(235.215/highway.MPG)) %>%
  arrange(-median.Enginge.HP) %>%
  head()

# Odp.: Dla kombinacji "flex-fuel (premium unleaded required/E85)" i "all wheel drive".
#       Średnie spalanie na autostradzie dla tej kombinacji: 12.3 L/100km.


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>%
  mutate(city.Lp100km = 235.215/city.mpg, highway.Lp100km = 235.215/highway.MPG) %>%
  group_by(Make, Model) %>%
  summarise(mean.diff = mean(city.Lp100km - highway.Lp100km)) %>%
  arrange(-mean.diff) %>%
  head()

# Odp.: Ferrari Enzo, średnia różnica 14.0 L/100km.
