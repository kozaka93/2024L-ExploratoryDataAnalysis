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

# Odp.:
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
  summarise(IQR = quantile(city.mpg, probs = 0.75) - quantile(city.mpg, probs = 0.25))

# Odp.: IQR = 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders == 4) %>% 
  group_by(Make) %>% 
  summarise(srednia = mean(MSRP)) %>% 
  arrange(-srednia) %>% 
  head(2)

# Odp.: Alfa Romeo  61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>% 
  filter(Make == "BMW") %>% 
  group_by(Driven_Wheels) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(2)

data %>%
  filter(Make == "BMW", Driven_Wheels == "rear wheel drive") %>%
  group_by(Model) %>% 
  summarise(czestosc = sum(Popularity)) %>% 
  arrange(-czestosc) %>% 
  head(2)

# Odp.: Z napędem na tył. 
#       Najczęściej występujący model dla tego napędu to BMW 1 Series


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make == "Volkswagen") %>% 
  filter(Number.of.Doors == 2 | Number.of.Doors == 4) %>% 
  group_by(Number.of.Doors) %>% 
  summarise(mediana = median(Engine.HP))

# Odp.: 2 drzwi - mediana = 170
#       4 drzwi - mediana = 200

# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>% 
  group_by(Year, Make) %>% 
  # aby przeliczyc spalanie paliwa należy obliczyć 235.21 / MPG
  # MPG - jednostka, w ktorej podane jest spalanie w danych
  summarise(srednia = mean(235.21 / city.mpg)) %>% 
  arrange(-srednia) %>% 
  head(3)


# Odp.: Bugatti z 2008 i 2009 roku, spalanie: 29.4 l/100 km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
srednio_popularnosc <- data %>%
  filter(Year >= 2007, Year <= 2017) %>%
  group_by(Vehicle.Style, Year) %>%
  summarise(popularnosc = mean(Popularity)) %>%
  arrange(Year, desc(popularnosc)) %>%
  group_by(Year) %>%
  filter(popularnosc == max(popularnosc))

srednio_popularnosc

srednio_popularnosc %>% 
  group_by(Vehicle.Style) %>% 
  summarise(czestotliwosc = n()) %>% 
  arrange(-czestotliwosc) %>% 
  head(4)

# Odp.: 

#    Vehicle.Style        Year     popularnosc
#    <chr>               <int>       <dbl>
# 1  Cargo Minivan        2007       2964.
# 2  Crew Cab Pickup      2008       2461.
# 3  Extended Cab Pickup  2009       3583.
# 4  Extended Cab Pickup  2010       3093 
# 5  Regular Cab Pickup   2011       5657 
# 6  Cargo Van            2012       5657 
# 7  Passenger Van        2012       5657 
# 8  Cargo Van            2013       5657 
# 9  Passenger Van        2013       5657 
# 10 Cargo Van            2014       2858.
# 11 Cargo Minivan        2015       4337 
# 12 Cargo Minivan        2016       4051.
# 13 Passenger Van        2017       5657 

# Najczęściej: Cargo Minivan, Cargo Van i Passenger Van


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
  filter(Market.Category == "Luxury,Performance", Year >= 2000) %>% 
  group_by(Make) %>% 
  summarise(mediana = median(MSRP)) %>% 
  arrange(-mediana) %>% 
  head(2)

# Volkswagen

data %>% 
  filter(Market.Category == "Luxury,Performance", Year >= 2000) %>% 
  group_by(Make) %>% 
  summarise(mediana = median(MSRP)) %>% 
  arrange(mediana) %>% 
  head(2)

# Hyundai

data %>% 
  filter(Market.Category == "Luxury,Performance", Year >= 2000) %>% 
  filter(Make == "Volkswagen" | Make == "Hyundai") %>% 
  group_by(Make) %>% 
  summarise(srednia_cylindrow = mean(Engine.Cylinders), srednia_mocy = mean(Engine.HP))
 
roznica_cylindry = 10.7 - 6
print(roznica_cylindry)

roznica_moc = 397 - 311
print(roznica_moc)


# Odp.: Najwyższa mediana - Volkswagen 
# (Volkswagen - średnia liczba cylindrów 10.7, średnia moc silnika 397)
# Najniższa mediana - Hyundai
# (Hyundai - średnia liczba cylindrów 6, średnia moc silnika 311)
# Różnica - cylindry: 4.7, Różnica- moc: 86


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>% 
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>% 
  summarise(kwantyl_0_1 = quantile(city.mpg, probs = 0.1), 
            kwantyl_0_9 = quantile(city.mpg, probs = 0.9))

# Odp.: 0.1 kwantyl 14, 0.9 kwantyl 25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>% 
  filter(Make == "Porsche", Engine.HP <= 300) %>% 
  group_by(Model) %>% 
  summarise(popularnosc = sum(Popularity)) %>% 
  arrange(-popularnosc) %>% 
  head(4)

# Odp.: Porsche 944 i Porsche Cayenne


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?

data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  summarise(mediana = median(Engine.HP, na.rm = TRUE), spalanie = 235.21 / mean(highway.MPG)) %>% 
  arrange(-mediana) %>% 
  head(2)


# Odp.: Dla typu paliwa flex-fuel (premium unleaded required/E85) i napędu na cztery koła
#       Spalanie 12.2 l/100 km


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>% 
  group_by(Make, Model) %>% 
  mutate(miasto = 235.21 / city.mpg, autostrada = 235.21 / highway.MPG) %>% 
  summarise(srednia_roznica = abs(miasto - autostrada)) %>% 
  arrange(-srednia_roznica) %>% 
  head(2)

# Odp.: Ferrari Enzo, różnica 14.0 l/100 km

