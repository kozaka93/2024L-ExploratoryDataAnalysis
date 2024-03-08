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
  summarise(med = median(Engine.HP, na.rm = TRUE))

# Odp.: 
#1 AUTOMATED_MANUAL    220
#2 AUTOMATIC           253
#3 DIRECT_DRIVE        147
#4 MANUAL              172
#5 UNKNOWN             125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>%
  filter(Make == "Mercedes-Benz" & Engine.Fuel.Type == "diesel") %>%
  summarise(kwartyle = quantile(city.mpg, na.rm = TRUE))

# Odp.: 26.25 - 19.00 = 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  group_by(Make) %>% 
  filter(Engine.Cylinders == 4) %>% 
  summarise(John_Cena = mean(MSRP)) %>% 
  arrange(desc(John_Cena))

# Odp.: 1 Alfa Romeo       61600 


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>% 
  group_by(Driven_Wheels) %>% 
  filter(Make == "BMW") %>% 
  summarise(n = n())

data %>% 
  group_by(Model) %>% 
  filter(Make == "BMW", Driven_Wheels == "rear wheel drive") %>% 
  summarise(n = n()) %>% 
  arrange(-n)
# Odp.: rear wheel drive    189, 1 Series               16


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>%
  filter(Make == "Volkswagen") %>% 
  group_by(Number.of.Doors) %>% 
  summarise(med = median(Engine.HP))

# Odp.:
#Number.of.Doors   med
#             2   170
#             3    90
#             4   200


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>% 
  group_by(Year,Make) %>% 
  filter(!is.na(city.mpg)) %>%
  summarise(sr_spalanie = 1/mean(city.mpg) * 3.78541178 * 100 / 1.609344) %>% 
  arrange(desc(sr_spalanie))


# Odp.: 2008 Bugatti            29.4


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?

z7 <- data %>% 
  filter(Year >= 2007 & Year <= 2017) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(sr = mean(Popularity)) %>% 
  group_by(Year) %>% 
  filter(sr == max(sr)) %>% 
  arrange(desc(Vehicle.Style))

z7 %>%

  group_by(Vehicle.Style) %>% 
  summarise(suma = n()) %>% 
  arrange(desc(suma))

z7

# Odp.:
#2011 - Regular Cab Pickup, 2012, 2013, 2017 - Passenger Van, 2009, 2010 - Extended Cab Pickup,
#2008 - Crew Cab Pickup, 2012, 2013, 2014 - Cargo Van, 2007, 2015, 2016 - Cargo Minivan

#Passenger Van, Cargo Van, Cargo Minivan


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
  group_by(Make) %>% 
  filter(Market.Category == "Luxury,Performance", Year >= 2000) %>% 
  summarise(med = median(MSRP)) %>% 
  arrange(desc(med))
  

data %>%
  group_by(Make) %>%
  filter(Make == "Volkswagen" | Make == "Hyundai" & Market.Category == "Luxury,Performance") %>%
  summarise(cyl = mean(Engine.Cylinders, na.rm = TRUE), kon = mean(Engine.HP, na.rm = TRUE))
  
  
# Odp.: Volkswagen    94600, 
#Make         cyl   kon
#Hyundai      6     311 
#Volkswagen   4.37  190.

# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>% 
  filter(Vehicle.Size == "Midsize", Transmission.Type == "AUTOMATIC") %>% 
  summarise(kwan01 = quantile(city.mpg, 0.1), kwan02 = quantile(city.mpg, 0.9))

# Odp.:  
#kwan01 kwan02
#    14     25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>% 
  filter(Make == "Porsche", Engine.HP <= 300) %>% 
  View()


# Odp.: Wszystkie Popularity są takie same, więc nie ma drugiego najbardziej popularnego modelu.


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  summarise(med = median(Engine.HP)) %>% 
  arrange(-med)

data %>% 
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)", Driven_Wheels == "all wheel drive") %>% 
  summarise(srednie = 1/mean(highway.MPG) * 3.78541178 * 100 / 1.609344)
  

# Odp.: flex-fuel (premium unleaded required/E85)    all wheel drive   608 
#  srednie
#  12.21894


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>% 
  group_by(Make, Model) %>% 
  summarise(roz = abs((1/mean(city.mpg) - 1/mean(highway.MPG)) * 3.78541178 * 100 / 1.609344)) %>% 
  arrange(desc(roz))

# Odp.: Ferrari     Enzo           14.0 

