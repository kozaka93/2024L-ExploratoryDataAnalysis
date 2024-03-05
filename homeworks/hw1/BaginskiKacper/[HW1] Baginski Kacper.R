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
  summarise(MHP = median(Engine.HP, na.rm = TRUE)) %>% 
  arrange(-MHP) # od Median Horse Power

# Odp.: auto: 253, auto/man: 220, man: 172, direct: 147, unknown: 125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>%
  filter(Make == "Mercedes-Benz" & Engine.Fuel.Type == "diesel") %>%
  summarise(ICFC = IQR(city.mpg, na.rm = TRUE)) # od IQR City Fuel Consumption

# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders == 4) %>% 
  group_by(Make) %>% 
  summarise(ASRP = mean(MSRP)) %>% 
  arrange(-ASRP) # od Average Suggested Retail Price

# Odp.: Alfa Romeo: 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>%
  filter(Make == "BMW") %>%
  group_by(Driven_Wheels) %>%
  count() %>%
  arrange(-n)
data %>% 
  filter(Make == "BMW" & Driven_Wheels == "rear wheel drive") %>%
  group_by(Model) %>% 
  count() %>% 
  arrange(-n)

# Odp.: najwięcej ma napęd na tył, 1 Series


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>%
  filter(Make == "Volkswagen") %>%
  group_by(Number.of.Doors) %>%
  summarise(MHP = median(Engine.HP, na.rm = TRUE))


# Odp.: 2 drzwi - 170 HP mediana, 4 drzwi - 200 HP mediana


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>%
  group_by(Year, Make) %>%
  summarise(ACMPG = mean(city.mpg, na.rm = TRUE)) %>%
  mutate(ACLPK = 235.21 / ACMPG) %>% 
  arrange(-ACLPK) 
# od Average City Miles per Gallon/Liters per 100 Kilometers

# Odp.: Bugatti 2008, 29.4 l/100km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
data %>%
  filter(2007<=Year & Year<=2017) %>% 
  group_by(Year, Vehicle.Style) %>%
  summarise(Count = n()) %>%
  arrange(Year, desc(Count)) %>%
  slice(1) %>%
  ungroup()

data %>%
  filter(2007<=Year & Year<=2017) %>% 
  group_by(Vehicle.Style) %>%
  count() %>% 
  arrange(-n)


# Odp.: Sedan w latach 10-13 i 15, 16, w pozostałych 4dr SUV,
# Najpopularniejszy ogółem Sedan


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?


data %>%
  filter(Market.Category == "Luxury,Performance", Year >= 2000) %>%
  group_by(Make) %>%
  summarise(Median_MSRP = median(MSRP, na.rm = TRUE),
            Avg_Cylinders = mean(Engine.Cylinders, na.rm = TRUE),
            Avg_Horsepower = mean(Engine.HP, na.rm = TRUE)) %>%
  arrange(-Median_MSRP)

# Odp.: najwyższą cenę ma Volkswagen
# Ma średnio o 4.7 cylindra więcej, i 86 Koni Mechanicznych więcej niż
# marka o najniższej cenie (Hyundai)


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>%
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>%
  summarise(Q10 = quantile(city.mpg, 0.1, na.rm = TRUE),
            Q90 = quantile(city.mpg, 0.9, na.rm = TRUE))

# Odp.: 0.1 kwantyl = 14 mpg, 0.9 kwantyl = 25 mpg


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>%
  filter(Make == "Porsche" & Engine.HP <= 300) %>%
  group_by(Model) %>%
  summarise(PopularityTotal = sum(Popularity)) %>%
  arrange(-PopularityTotal)

# Odp.: Drugim najpopularniejszym modelem jest 944


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>%
  mutate(HLPK = 235.21 / highway.MPG) %>% 
  mutate(MHP = median(Engine.HP)) %>% 
  arrange(-MHP) %>% 
  select(Engine.Fuel.Type, Driven_Wheels, MHP, HLPK)

# Odp.: flex-fuel (premium unleaded required/E85), all wheel drive
# spala 13.8 L/100


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>%
  mutate(HLPK = 235.21 / highway.MPG, CLPK = 235.21 / city.mpg) %>% 
  group_by(Make, Model) %>%
  summarise(AD = mean(abs(HLPK - CLPK), na.rm = TRUE)) %>%
  arrange(-AD)


# Odp.: Ferrari Enzo spala aż 14 L/100 więcej w mieście niż na autostradzie 

