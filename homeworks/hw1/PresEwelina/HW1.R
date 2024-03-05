
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
# Transmission.Type mediana
# <chr>               <dbl>
# 1 AUTOMATED_MANUAL      220
# 2 AUTOMATIC             253
# 3 DIRECT_DRIVE          147
# 4 MANUAL                172
# 5 UNKNOWN               125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>% 
  filter(Engine.Fuel.Type == "diesel") %>% 
  group_by(Make) %>% 
  summarise(IQR = IQR(city.mpg)) %>% 
  filter(Make == "Mercedes-Benz")

# Odp.:
#Rozstęp międzykwartylowy: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders == 4) %>% 
  group_by(Make) %>% 
  summarise(Av.MSRP = mean(MSRP)) %>% 
  arrange(-Av.MSRP)

# Odp.:
#Alfa Romeo, 61600 


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

# Odp.:
#więcej jest BMW z napędem na tył (rear wheel drive)
#najczęściej występujący model dla tego napędu: 1 Series


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make == "Volkswagen") %>% 
  group_by(Number.of.Doors) %>% 
  summarise(Median.HP = median(Engine.HP)) %>% 
  filter(Number.of.Doors %in% c(2, 4))

# Odp.:
# Number.of.Doors   Median.HP
#               2         170
#               4         200

#Dla 2 drzwi mediana mocy silnika wynosi 170,
#Dla 4 drzwi wynosi 200, czyli o 30 więcej


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

#Mamy podane spalanie w milach na galon
#1 US galon = 3,785 litra
#1 mila amerykańska = 1,609 km

data %>% 
  group_by(Year, Make) %>% 
  summarise(Av.L100 = mean((1/(city.mpg*1.609/3.785)*100))) %>% 
  arrange(-Av.L100)

# Odp.:
#Bugatti z 2008 i 2009 roku
#29.4L/100km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?

(data %>%
   filter(Year %in% 2007:2017) %>%
   group_by(Year, Vehicle.Style) %>%
   summarise(Av.Popularity = mean(Popularity), .groups = 'drop') %>%
   group_by(Year) %>%
   slice_max(order_by = Av.Popularity, n = 1) -> data1)         

data1 %>% 
  group_by(Vehicle.Style) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

# Odp.:
#     Year Vehicle.Style               Av.Popularity
# 1   2007 Cargo Minivan               2964.
# 2   2008 Crew Cab Pickup             2461.
# 3   2009 Extended Cab Pickup         3583.
# 4   2010 Extended Cab Pickup         3093 
# 5   2011 Regular Cab Pickup          5657 
# 6   2012 Cargo Van                   5657 
# 7   2012 Passenger Van               5657 
# 8   2013 Cargo Van                   5657 
# 9   2013 Passenger Van               5657 
# 10  2014 Cargo Van                   2858.
# 11  2015 Cargo Minivan               4337 
# 12  2016 Cargo Minivan               4051.
# 13  2017 Passenger Van               5657

# Vehicle.Style             n
# 1 Cargo Minivan           3
# 2 Cargo Van               3
# 3 Passenger Van           3
# 4 Extended Cab Pickup     2
# 5 Crew Cab Pickup         1
# 6 Regular Cab Pickup      1

#Najbardziej popularnymi stylami były Cargo Minivan, Cargo Van, Passenger Van


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
  filter(Market.Category == "Luxury,Performance", Year > 2000) %>% 
  group_by(Make) %>% 
  summarise(Median.MSRP = median(MSRP)) %>% 
  arrange(-Median.MSRP)

data %>% 
  filter(Market.Category == "Luxury,Performance", Make %in% c("Volkswagen", "Hyundai")) %>% 
  group_by(Make) %>% 
  summarise(Av.Cylinders = mean(Engine.Cylinders, na.rm = TRUE), Av.HP = mean(Engine.HP, na.rm = TRUE))


# Odp.:
#Najwyższa mediana: Volkswagen
#Najniższa mediana: Hyundai

# Make       Av.Cylinders Av.HP
# Hyundai             6     311
# Volkswagen         10.7   397

#Różnica śr. liczby cylindrów: 4.7
#Różnica śr. liczby koni mechanicznych: 86


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>% 
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>% 
  summarise(kwantyl0.1 = quantile(city.mpg, probs = 0.1),
            kwantyl0.9 = quantile(city.mpg, probs = 0.9))

# Odp.:
# kwantyl0.1    kwantyl0.9
#         14            25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>% 
  filter(Make == "Porsche", Engine.HP <= 300) %>% 
  group_by(Model) %>%
  summarise(Av.Popularity = mean(Popularity))

# Odp.:
#Wszystkie modele Porsche są tak samo popularne


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?

data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  summarise(Median.HP = median(Engine.HP, na.rm = TRUE), .groups = "drop") %>% 
  arrange(-Median.HP)

data %>% 
  mutate(Highway.L100 = 1/(highway.MPG*1.609/3.785)*100) %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  summarise(Av.Highway.L100 = mean(Highway.L100, na.rm = TRUE), .groups = "drop") %>% 
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)", Driven_Wheels == "all wheel drive")

# Odp.:
#flex-fuel (premium unleaded required/E85)    all wheel drive          608 
#Średnie spalanie na autostradzie w L/100km:  12.3


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

#1 US galon = 3,785 litra
#1 mila amerykańska = 1,609 km

data %>% 
  mutate(City.L100 = 1/(city.mpg*1.609/3.785)*100) %>% 
  mutate(Highway.L100 = 1/(highway.MPG*1.609/3.785)*100) %>% 
  group_by(Make, Model) %>% 
  summarise(Diff.L100 = mean(abs(City.L100-Highway.L100)), .groups = 'drop') %>% 
  arrange(-Diff.L100)

# Odp.:
#Ferrari     Enzo        14.0
