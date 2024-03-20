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
  summarise(median_hp = median(Engine.HP, na.rm = TRUE))

# Odp.: AUTOMATED_MANUAL - 220, AUTOMATIC - 253, DIRECT_DRIVE - 147, MANUAL - 172,
# UNKNOWN - 125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>% 
  filter(Make == "Mercedes-Benz" & Engine.Fuel.Type == "diesel") %>% 
  summarise(iqr_mpg = IQR(city.mpg, na.rm = TRUE))

# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders == 4) %>% 
  group_by(Make) %>% 
  summarise(mean_MSRP = mean(MSRP, na.rm = TRUE)) %>% 
  arrange(-mean_MSRP)

# Odp.: Alfa Romeo; 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.

data %>% 
  filter(Make == "BMW") %>% 
  group_by(Driven_Wheels) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

data %>% 
  filter(Make == "BMW" & Driven_Wheels == "rear wheel drive") %>% 
  group_by(Model) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

# Odp.: Więcej jest aut z napędem na tył (rear wheel drive); najczęściej występuje
# model 1 Series 


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make == "Volkswagen") %>% 
  group_by(Number.of.Doors) %>% 
  summarise(median_hp = median(Engine.HP, na.rm = TRUE))
  
# Odp.: Dla 2 drzwi mediana wynosi 170, dla 4 drzwi mediana wynosi 200


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>% 
  group_by(Year, Make) %>% 
  summarise(mean_mpg = mean(city.mpg, na.rm = TRUE)) %>% 
  mutate(mean_l100km = mean_mpg/235.21) %>% 
  arrange(-mean_l100km)

# Odp.: Tesla, 2016; spalanie wynosi 0.411 L/100km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
df <- data %>% 
  filter(Year >= 2007 & Year <= 2017) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(mean_popularity = mean(Popularity, na.rm = TRUE)) %>% 
  group_by(Year) %>% 
  slice(which.max(mean_popularity)) 
 
df %>% 
  group_by(Vehicle.Style) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

# Odp.: 2007 - Cargo Minivan, 2008 - Crew Cab Pickup, 2009 - Extended Cab Pickup, 
# 2010 - Extended Cab Pickup, 2011 - Regular Cab Pickup, 2012 - Cargo Van, 
# 2013 - Cargo Van, 2014 - Cargo Van, 2015 - Cargo Minivan, 2016 - Cargo Minivan,
# 2017 - Passenger Van; najczęściej przez te 10 lat wystąpiły Cargo Minivan i Cargo Van


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
  filter(Market.Category == "Luxury,Performance" & Year >= 2000) %>% 
  group_by(Make) %>% 
  summarise(median_MRSP = median(MSRP, na.rm = TRUE)) %>% 
  arrange(-median_MRSP)

data %>% 
  filter(Market.Category == "Luxury,Performance" & Year >= 2000 & 
           Make %in% c("Volkswagen", "Hyundai")) %>%
  group_by(Make) %>% 
  summarise(mean_eng.cyl = mean(Engine.Cylinders, na.rm = TRUE), 
            mean_eng.hp = mean(Engine.HP, na.rm = TRUE))

# Odp.: Volkswagen; średnia liczba cylindrów dla marki z najwyższą medianą sugerowanej 
# ceny detalicznej wynosi 10.7, z najniższą - 6; średnia moc silnika dla marki z najwyższą
# medianą sugerowanej ceny detalicznej wynosi 397, z najniższą - 311


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>% 
  filter(Transmission.Type == "AUTOMATIC" & Vehicle.Size == "Midsize") %>% 
  reframe(quantile_city.mpg = quantile(city.mpg, probs = c(0.1, 0.9), na.rm = TRUE)) 

# Odp.: 0.1 kwantyl wynosi 14, 0.9 kwantyl wynosi 25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>% 
  filter(Make == "Porsche" & Engine.HP <= 300) %>% 
  select(Model, Year, Popularity) %>% 
  arrange(-Popularity)

# Odp.: Wszystkie modele spełniające zadane warunki są tak samo popularne


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  summarise(median_hp = median(Engine.HP, na.rm = TRUE)) %>% 
  arrange(-median_hp)

data %>% 
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)" &
           Driven_Wheels == "all wheel drive") %>% 
  summarise(mean_highway.MPG = mean(highway.MPG, na.rm = TRUE)) %>% 
  transmute(mean_highway.l100km = mean_highway.MPG/235.21)

# Odp.: flex-fuel (premium unleaded required/E85) & all wheel drive; średnie spalanie
# wynosi 0.08184176 L/100km


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>% 
  group_by(Make, Model) %>% 
  mutate(diff.city_highway = abs((highway.MPG - city.mpg))/235.21) %>% 
  summarise(mean_diff = mean(diff.city_highway, na.rm = TRUE)) %>% 
  arrange(-mean_diff)

# Odp.: Kia & Soul EV; różnica wynosi 0.119 L/100km

