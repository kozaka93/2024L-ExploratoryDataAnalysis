library(dplyr)

data <- read.csv("data.csv")
data %>% 
  View()

str(data)


# Zadanie 0 -data# Zadanie 0 ---------------------------------------------------------------
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
  summarise(median_HP = median(Engine.HP, na.rm = TRUE)) %>%
  arrange(-median_HP)


# Odp.: mediana mocy podana w koniach mechanicznych
# 1 AUTOMATIC            253
# 2 AUTOMATED_MANUAL     220
# 3 MANUAL               172
# 4 DIRECT_DRIVE         147
# 5 UNKNOWN              125



# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>%
  filter(Make == "Mercedes-Benz", Engine.Fuel.Type == "diesel") %>% 
  summarise(rozstep = IQR(city.mpg))

# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders == 4) %>% 
  group_by(Make) %>% 
  summarise(mean_price = mean(MSRP)) %>% 
  arrange(-mean_price) %>% 
  head(1)


# Odp.: Alfa Romeo podaje najwyższą średnią sugerowaną cenę detaliczną i jest
# to 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.

data %>% 
  filter(Make == "BMW") %>%
  group_by(Driven_Wheels) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(1)

data %>% 
  filter(Make == "BMW", Driven_Wheels == "rear wheel drive") %>%
  group_by(Model) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(1)

# Odp.: najwięcej jest samochodów z napędem na tylne koła. A najczęściej występującym
# modelem BMW z takim napędem jest BMW 1 Series.

# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make == "Volkswagen") %>%
  group_by(Number.of.Doors) %>% 
  summarise(median_HP = median(Engine.HP, na.rm = TRUE)) %>% 
  arrange(-median_HP) 


# Odp.: mediana mocy silnika samochodów 4 drzwiowych to 200 koni mechanicznych,
# mediana mocy silnika samochodów 2 drzwiowych to 170 koni mechanicznych.


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>% 
  mutate(fuel_usage_city = 1 / city.mpg * 3.785 / 1.609 * 100 ) %>% 
  #1 gallon ~ 3.785 litrów, 1 mila ~ 1.609 km
  group_by(Year, Make) %>% 
  summarise(mean_fuel_usage_city = mean(fuel_usage_city, na.rm = TRUE)) %>% 
  arrange(-mean_fuel_usage_city) %>%  
  head(3)

# Odp.: Bugatti z 2008 oraz z 2009 roku (ex aequo) mają najwyższe średnie spalanie w mieście i wynosi ono 29.4 L/100km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
data %>% 
  filter(2017 >= Year & Year >= 2007) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(style_mean_popularity = mean(Popularity, na.rm = TRUE)) %>% 
  group_by(Year) %>% 
  filter(style_mean_popularity == max(style_mean_popularity)) %>% 
  select(Year, Vehicle.Style)

data %>% 
  filter(2017 >= Year & Year >= 2007) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(style_mean_popularity = mean(Popularity, na.rm = TRUE)) %>% 
  group_by(Year) %>% 
  filter(style_mean_popularity == max(style_mean_popularity)) %>% 
  select(Year, Vehicle.Style) %>%
  group_by(Vehicle.Style) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

# Odp.:
# 2007 Cargo Minivan      
# 2008 Crew Cab Pickup    
# 2009 Extended Cab Pickup
# 2010 Extended Cab Pickup
# 2011 Regular Cab Pickup 
# 2012 Cargo Van          
# 2012 Passenger Van      
# 2013 Cargo Van          
# 2013 Passenger Van      
# 2014 Cargo Van          
# 2015 Cargo Minivan      
# 2016 Cargo Minivan      
# 2017 Passenger Van 

# Cargo Minivan, Cargo Van i Passenger Van wystąpiły po 3 razy każdy, pozostałe
# style samochodów były średnio najbardziej popularne mniej razy.

# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
  filter(Market.Category == "Luxury,Performance", Year >= 2000) %>% 
  group_by(Make) %>% 
  summarise(price_median = mean(MSRP, na.rm = TRUE)) %>% 
  arrange(-price_median)

# jak rozumiem mam tu porównać średnie parametry silnika marki Volskwagen
# i marki Volvo dla samochodów z tej kategorii.

data %>% 
  filter(Market.Category == "Luxury,Performance", Year >= 2000,
         Make == "Volvo" | Make == "Volkswagen") %>%
  group_by(Make) %>% 
  summarise(mean_cylinders = mean(Engine.Cylinders, na.rm = TRUE),
            mean_hp = mean(Engine.HP, na.rm = TRUE))



# Odp.: są to samochody marki Volkswagen. Spośród samochodów tej kategorii 
# Volkswagen ma średnio 10.7 cylindrów i 397 koni mechanicznych, 
# a Volvo 4.57 cylindrów i 262 koni mechanicznych.


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>% 
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>%
  mutate(fuel_usage_city = 1 / city.mpg * 3.785 / 1.609 * 100 ) %>%
  transmute(kwantyl_01 = quantile(fuel_usage_city, 0.1), 
         kwantyl_09 = quantile(fuel_usage_city, 0.9)) %>% 
  head(1)
  
# Odp.: kwantyl 0.1 to 9.409571 a kwantyl 0.9 to 16.80281


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>% 
  filter(Make == "Porsche", Engine.HP <= 300) %>% 
  select(Model, Popularity) %>% 
  arrange(-Popularity) %>% 
  View()

# Odp.: Wszystkie modele marki Porsche o mocy nie większej niż 300 koni mechanicznych
# mają taką samą popularność równą 1715 (co jest dosyć podejrzane).


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?

data %>%
  group_by(Engine.Fuel.Type, Driven_Wheels) %>%
  summarise(median_HP = median(Engine.HP, na.rm = TRUE)) %>%
  arrange(-median_HP) %>% 
  head(1)

data %>%
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)", 
         Driven_Wheels == "all wheel drive") %>%
  mutate(fuel_usage_highway = 1 / highway.MPG * 3.785 / 1.609 * 100 ) %>%
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  summarise(mean_fuel_usage_highway = mean(fuel_usage_highway, na.rm = TRUE))
  

# Odp.: Mediana ta jest największa dla napędu na 4 koła i paliwa flex_fuel (premium unleaded required/E85)
# Średnie spalanie dla tej kombinacji na autostradzie wynosi 12.3 litrów na 100 km.


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>% 
  mutate(fuel_usage_difference = abs(1 / city.mpg * 3.785 / 1.609 * 100 - 1 / highway.MPG * 3.785 / 1.609 * 100)) %>% 
  # moduł raczej nic tu nie wnosi, bo wątpię by jakiś samochód palił więcej na autostradzie.
  group_by(Make, Model) %>%
  summarise(mean_fuel_usage_difference = mean(fuel_usage_difference, na.rm = TRUE)) %>% 
  arrange(-mean_fuel_usage_difference)

# Odp.: Jest to Ferrari Enzo. Różnica w średnim spalaniu wynosi 14 L/100km