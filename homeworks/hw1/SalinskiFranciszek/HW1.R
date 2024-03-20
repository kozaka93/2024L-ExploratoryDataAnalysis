library(dplyr)

dir_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir_path)
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
  summarise(median_HP = median(Engine.HP, na.rm=TRUE))

# Odp.:
# Transmission.Type         median_HP
# 1 AUTOMATED_MANUAL        220
# 2 AUTOMATIC               253
# 3 DIRECT_DRIVE            147
# 4 MANUAL                  172
# 5 UNKNOWN                 125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>% 
  filter(Make == "Mercedes-Benz", Engine.Fuel.Type == "diesel") %>%
  summarise(Q1 = quantile(city.mpg, 1/4, na.rm=TRUE),
            Q3 = quantile(city.mpg, 3/4, na.rm=TRUE),
            IQR = IQR(city.mpg, na.rm=TRUE))

# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>%
  filter(Engine.Cylinders == 4) %>%
  group_by(Make) %>%
  summarise(mean_price = mean(MSRP, na.rm=TRUE)) %>%
  arrange(-mean_price)

# Odp.: Alfa Romeo 61600


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

# Odp.: Więcej jest z napędem na tylne koła, 
# najczęściej występujący model to 1 Series


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>%
  filter(Make == "Volkswagen") %>%
  group_by(Number.of.Doors) %>%
  filter(Number.of.Doors %in% c(2, 4)) %>%
  summarise(median_HP = median(Engine.HP, na.rm=TRUE))

# Odp.:   Number.of.Doors median_HP
#         2               170
#         4               200


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

# 1 galon = 3.785 L
# 1 mila = 1.609 km

data %>%
  group_by(Make, Year) %>%
  mutate(city.LP100km = 100 / (city.mpg * 1.609 / 3.785)) %>%
  summarise(mean_city_LP100km = mean(city.LP100km, na.rm=TRUE)) %>%
  arrange(-mean_city_LP100km)

# Odp.: Bugatti 2008/2009 29.4L/100km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
(data %>%
  filter(Year %in% 2007:2017) %>%
  group_by(Year, Vehicle.Style) %>%
  summarise(style_popularity = mean(Popularity, na.rm = TRUE), .groups = "drop") %>%
  group_by(Year) %>%
  slice_max(order_by = style_popularity, n = 1) -> style_popularity_data)

style_popularity_data %>%
  group_by(Vehicle.Style) %>%
  summarise(n = n()) %>%
  arrange(-n)

# Odp.:
#     Year  Vehicle.Style        style_popularity
# 1   2007  Cargo Minivan        2964.
# 2   2008  Crew Cab Pickup      2461.
# 3   2009  Extended Cab Pickup  3583.
# 4   2010  Extended Cab Pickup  3093 
# 5   2011  Regular Cab Pickup   5657 
# 6   2012  Cargo Van            5657 
# 7   2012  Passenger Van        5657 
# 8   2013  Cargo Van            5657 
# 9   2013  Passenger Van        5657 
# 10  2014  Cargo Van            2858.
# 11  2015  Cargo Minivan        4337 
# 12  2016  Cargo Minivan        4051.
# 13  2017  Passenger Van        5657
#
# Najczesciej jako najbardziej popularne wystapily Cargo Minivan, Cargo Van, Passenger Van  


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>%
  filter(Market.Category == "Luxury,Performance", Year >= 2000) %>%
  group_by(Make) %>%
  summarise(median_MSRP = median(MSRP, na.rm=TRUE),
            mean_cylinders = mean(Engine.Cylinders, na.rm=TRUE),
            mean_HP = mean(Engine.HP, na.rm=TRUE)) %>%
  arrange(-median_MSRP)

# Odp.: Najwyzsza mediane ma Volskwagen, najnizsza Hyundai
#    Make          median_MSRP mean_cylinders mean_HP
# 1  Volkswagen    94600       10.7           397
# ...
# 15 Hyundai       39625       6              311
#
# Roznica w sredniej liczbie cylindrow: 4.7
# Roznica w sredniej mocy silnika: 86


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>%
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>%
  summarise(Q01 = quantile(city.mpg, 0.1, na.rm=TRUE),
            Q09 = quantile(city.mpg, 0.9, na.rm=TRUE))

# Odp.: Q01 Q09
#       14  25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>%
  filter(Make == "Porsche", Engine.HP <= 300) %>%
  group_by(Model) %>%
  summarise(mean_popularity = mean(Popularity)) %>%
  arrange(-mean_popularity)

# Odp.: Wszystkie sa srednio tak samo popularne


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>%
  group_by(Engine.Fuel.Type, Driven_Wheels) %>%
  mutate(highway.LP100km = 100 / (highway.MPG * 1.609 / 3.785)) %>%
  summarise(median_HP = median(Engine.HP, na.rm=TRUE),
            mean_highway_LP100km = mean(highway.LP100km, na.rm=TRUE)) %>%
  arrange(-median_HP)

# Odp.: Engine.Fuel.Type                             Driven_Wheels    median_HP mean_highway_LP100km
#       flex-fuel (premium unleaded required/E85)    all wheel drive  608       12.3
# Srednie spalanie na autostradzie dla tej kombinacji to 12.3L/100km


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>%
  group_by(Make, Model) %>%
  mutate(city.LP100km = 100 / (city.mpg * 1.609 / 3.785),
         highway.LP100km = 100 / (highway.MPG * 1.609 / 3.785)) %>%
  summarise(mean_diff_LP100 = mean(abs(city.LP100km - highway.LP100km), na.rm=TRUE)) %>%
  arrange(-mean_diff_LP100)

# Odp.: Ferrari Enzo, roznica wynisi 14L/100km
