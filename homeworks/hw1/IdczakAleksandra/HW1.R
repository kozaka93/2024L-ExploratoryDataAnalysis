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
# Transmission.Type   mediana
# 1 AUTOMATED_MANUAL      220
# 2 AUTOMATIC             253
# 3 DIRECT_DRIVE          147
# 4 MANUAL                172
# 5 UNKNOWN               125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>% 
  filter(Make == "Mercedes-Benz" & Engine.Fuel.Type == "diesel") %>% 
  summarise(rm = IQR(city.mpg, na.rm = TRUE))

# Odp.: 7.25.


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders == "4") %>% 
  group_by(Make) %>% 
  summarise(srednia = mean(MSRP)) %>% 
  arrange(-srednia) %>% 
  head(1)

# Odp.: Alfa Romeo, srednia cena: 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>% 
  filter(Make == "BMW") %>% 
  group_by(Driven_Wheels) %>% 
  summarise(n = n())

data %>% 
  filter(Make == "BMW" & Driven_Wheels == "rear wheel drive") %>% 
  group_by(Model) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)

# Odp.: Więcej jest aut BMW z napędem na tył. Najczęściej występujący model z 
# napędem na tylne koła to 1 Series.


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make == "Volkswagen") %>% 
  group_by(Number.of.Doors) %>% 
  summarise(mediana = median(Engine.HP))

# Odp.: Dla 2 drzwi mediana 170, dla 4 drzwi mediana 200.


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>% 
  mutate(city.mpg.l100 = 235.2145 / city.mpg) %>%
  group_by(Make, Year) %>% 
  summarise(srednia = mean(city.mpg.l100)) %>% 
  arrange(-srednia) %>% 
  head()

# Odp.: Bugatti, 2008 i 2009, spalanie 29.4 L/100km
#   Make         Year srednia
# 1 Bugatti      2008    29.4
# 2 Bugatti      2009    29.4
# 3 Ferrari      2001    27.8
# 4 Lamborghini  2009    27.8
# 5 Lamborghini  2008    27.4
# 6 Ferrari      2005    26.1


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?

data %>% 
  filter(Year > 2006 & Year < 2018) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(srednia = mean(Popularity)) %>% 
  group_by(Year) %>% 
  filter(srednia == max(srednia))

data %>% 
  filter(Year > 2006 & Year < 2018) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(srednia = mean(Popularity)) %>% 
  group_by(Year) %>% 
  filter(srednia == max(srednia)) %>% 
  group_by(Vehicle.Style) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

# Odp.: Przez te lata najczęściej jako średnio najbardziej popularnymy styl wystąpił:
# Cargo Minivan, Cargo Van i Passenger Van (druga ramka).
#
#     Year Vehicle.Style       srednia
#  1  2007 Cargo Minivan         2964.
#  2  2008 Crew Cab Pickup       2461.
#  3  2009 Extended Cab Pickup   3583.
#  4  2010 Extended Cab Pickup   3093 
#  5  2011 Regular Cab Pickup    5657 
#  6  2012 Cargo Van             5657 
#  7  2012 Passenger Van         5657 
#  8  2013 Cargo Van             5657 
#  9  2013 Passenger Van         5657 
# 10  2014 Cargo Van             2858.
# 11  2015 Cargo Minivan         4337 
# 12  2016 Cargo Minivan         4051.
# 13  2017 Passenger Van         5657
#
#   Vehicle.Style           n
# 1 Cargo Minivan           3
# 2 Cargo Van               3
# 3 Passenger Van           3
# 4 Extended Cab Pickup     2
# 5 Crew Cab Pickup         1
# 6 Regular Cab Pickup      1


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
  filter(Year > 1999 & Market.Category == "Luxury,Performance") %>% 
  group_by(Make) %>% 
  summarise(mediana = median(MSRP)) %>% 
  arrange(-mediana)

data %>% 
  filter(Year > 1999 & Market.Category == "Luxury,Performance") %>% 
  filter(Make == "Volkswagen" | Make == "Hyundai") %>% 
  group_by(Make) %>% 
  summarise(s_moc = mean(Engine.HP, na.rm = TRUE), s_cylindry = mean(Engine.Cylinders, na.rm = TRUE))


# Odp.: Najwyższa mediana sugerowanej ceny - Volkswagen.
# Najniższą medianę ma Hyundai.  Marka z najwyższą medianą ma srednią moc silnika 397,
# a z najniższą 311. Srednia liczba cylindrów dla marki z najwyższą medianą - 10.7, 
# z najniższą - 6.


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>% 
  filter(Transmission.Type == "AUTOMATIC" & Vehicle.Size == "Midsize") %>% 
  summarise(kw_0.1 = quantile(city.mpg, 0.1), kw_0.9 = quantile(city.mpg, 0.9))

# Odp.: 0.1 kwantyl = 14, 0.9 kwantyl = 25.


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

# Gdy mierzymy popularność na przestrzeni lat:
data %>% 
  filter(Make == "Porsche" & Engine.HP <= 300) %>% 
  group_by(Model) %>% 
  summarise(p = sum(Popularity, na.rm = TRUE)) %>% 
  arrange(-p)

# Odp.: Na drugim miejscu znajdują się dwa modele (ich popularność jest taka sama),
# są to: 944 i Cayenne. 

# Gdy rozróżniamy rok produkcji modeli:
data %>% 
  filter(Make == "Porsche" & Engine.HP <= 300) %>% 
  arrange(-Popularity) %>% 
  select(Model, Year, Popularity)

# Odp.: Popularność wszystkich modeli jest taka sama.


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?

data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  summarise(mediana = median(Engine.HP, na.rm = TRUE)) %>% 
  arrange(-mediana)

data %>% 
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)" & Driven_Wheels == "all wheel drive") %>% 
  mutate(highway.mpg.l100 = 235.2145 / highway.MPG) %>%
  summarise(srednia = mean(highway.mpg.l100, na.rm = TRUE))


# Odp.: Mediana jest największa dla kombinacji: flex-fuel (premium unleaded required/E85) i
# all wheel drive. Średnie spalanie wynosi 12.26251.


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>% 
  mutate(highway.mpg.l100 = 235.2145 / highway.MPG, city.mpg.l100 = 235.2145 / city.mpg) %>%
  group_by(Make, Model) %>% 
  summarise(srednia_roznica = abs(mean(city.mpg.l100 - highway.mpg.l100))) %>% 
  arrange(-srednia_roznica) %>% 
  head()

# Odp.: Ferrari Enzo, roznica wynosi 14 L/100km.

