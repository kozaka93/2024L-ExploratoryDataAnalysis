library(dplyr)
library(tidyr)

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
  summarise(med_engine_HP = median(Engine.HP, na.rm = TRUE))


# Odp.:
# 1 AUTOMATED_MANUAL            220
# 2 AUTOMATIC                   253
# 3 DIRECT_DRIVE                147
# 4 MANUAL                      172
# 5 UNKNOWN                     125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>% 
  filter(Make == "Mercedes-Benz", Engine.Fuel.Type == "diesel") %>%
  select(city.mpg) %>% 
  summarise(IQR = quantile(city.mpg)[4] - quantile(city.mpg)[2])

# Odp.: IQR = 7,25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders == 4) %>% 
  group_by(Make) %>% 
  summarise(mean_MSRP = mean(MSRP, na.rm = TRUE)) %>%
  arrange(-mean_MSRP) %>% 
  head(1)

# Odp.: Alfa Romeo: 61600


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
# Najwięcej jest Aut BMW z napędem na tył (189)
# Najczęściej występujący model dla tego rodzaju napędu to 1 Series      


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make == "Volkswagen") %>% 
  filter(Number.of.Doors == 2 | Number.of.Doors == 4) %>% 
  group_by(Number.of.Doors) %>% 
  summarise(med_engine_HP = median(Engine.HP, na.rm = TRUE))

# Odp.: 
# Mediana mocy silnika dla 2 drzwi to 170, a dla 4 drzwi 200


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>% 
  group_by(Make, Year) %>% 
  summarise(mean_city_mpg = mean(city.mpg, na.rm = TRUE)) %>%
  mutate(gal_per_1mile = 1/mean_city_mpg) %>% 
  mutate(l_per_100km = 3785/1.609 * gal_per_1mile) %>% 
  arrange(-l_per_100km) %>% 
  head(10)


# Odp.: Bugatti z 2008 i 2009 roku - około 294l/ 100 km 


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?

data1 <- data %>% 
  filter(Year > 2006 & Year < 2018) %>%
  select(Year, Vehicle.Style, Popularity) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(mean_popularity = mean(Popularity, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Year, values_from = mean_popularity, values_fill = 0)

data1 %>% 
  mutate(mean_popularity =  rowSums(pick(where(is.numeric)))/11) %>% 
  arrange(-mean_popularity) %>% 
  head(1)

# Odp.: Średnio przez te 10 lat najbardziej popularny był Passenger Van (Popularity = 2454)


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
  filter(Year >= 2000 & Market.Category == "Luxury,Performance") %>% 
  group_by(Make) %>% 
  summarise(med_MSRP = median(MSRP, na.rm = TRUE)) %>%
  arrange(-med_MSRP)

data %>% 
  filter(Year >= 2000 & Market.Category == "Luxury,Performance" &
         Make == c("Volkswagen", "Hyundai")) %>% 
  group_by(Make) %>% 
  summarise(mean_engine_cylinders = mean(Engine.Cylinders, na.rm = TRUE),
            mean_engine_HP = mean(Engine.HP, na.rm = TRUE))

# Odp: 
# Najwyższa mediana - Volkswagen
# Najniższa mediana - Hyundai
# Volkswagen: cylindry 11.2 ; HP - 407.8
# Hyundai : cylindry 6.0 ; HP - 311


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>% 
  filter(Transmission.Type == "AUTOMATIC" & Vehicle.Size == "Midsize") %>% 
  summarise(quantiles = quantile(city.mpg, probs = c(0.1, 0.9), na.rm = TRUE))
  
# Odp.: 0.1 kwantyl - 14 mpg
#       0.9 kwantyl - 25 mpg 


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>% 
  filter(Make == "Porsche" & Engine.HP <= 300) %>% 
  group_by(Model) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(2)

# Odp.: Model 944


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?

data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  summarise(med_engine_HP = median(Engine.HP, na.rm = TRUE)) %>% 
  arrange(-med_engine_HP) %>% 
  head(1)

# flex-fuel (premium unleaded required/E85),  all wheel drive 
data %>% 
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)",
         Driven_Wheels == "all wheel drive") %>% 
  summarise(mean_highway_mpg = mean(highway.MPG, na.rm = TRUE)) %>% 
  mutate(gal_per_1mile = 1/mean_highway_mpg) %>% 
  mutate(l_per_100km = 3785/1.609 * gal_per_1mile)
  

# Odp.: 
# Największa mediana HP (608):
#         paliwo- flex-fuel (premium unleaded required/E85) ;
#         napęd-  all wheel drive          
# Średnie spalanie na autostradzie wynosi około 122,2 L/100 km


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>%
  filter(!is.na(city.mpg) & !is.na(highway.MPG)) %>%
  select(Make, Model, city.mpg, highway.MPG) %>% 
  mutate(city_Lp100km = 3785/(1.609 * city.mpg),
         highway_Lp100km = 3785/(1.609 * highway.MPG)) %>% 
  mutate(diff_Lp100 = abs(city_Lp100km - highway_Lp100km)) %>% 
  arrange(-diff_Lp100) %>% 
  head(1)
  

# Odp.: Ferrari Enzo - ta różnica wynosi około 140L/100km

