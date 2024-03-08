library(dplyr)

data <- read.csv("C:/Users/kowal/Desktop/Wstep do eksploracyji/data.csv")


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
  


# Odp.: 1 AUTOMATED_MANUAL      220
# 2 AUTOMATIC             253
# 3 DIRECT_DRIVE          147
# 4 MANUAL                172
# 5 UNKNOWN               125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>%
  filter(Make == "Mercedes-Benz" & Engine.Fuel.Type == "diesel") %>%
  summarize(quan = quantile(city.mpg, na.rm = TRUE))

# Odp.: 26.25 - 19.00 = 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>%
  filter(Engine.Cylinders == 4) %>%
  group_by(Make) %>%
  summarize(mean = mean(MSRP)) %>%
  arrange(desc(mean)) %>%
  head(1)


# Odp.: Alfa Romeo, cena: 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>%
  filter(Make == "BMW") %>%
  group_by(Driven_Wheels) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  head(1)

data %>%
  filter(Make == "BMW" & Driven_Wheels == "rear wheel drive") %>%
  group_by(Model) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(1)

# Odp.: Najwięcej jest samochodów BMW z napędem na tył.
# Najczęściej występujący model to 1 Series.

# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>%
  filter(Make == "Volkswagen" & Number.of.Doors == c(2, 4)) %>%
  group_by(Number.of.Doors) %>%
  summarise(median = median(Engine.HP))


# Odp.:
# Number.of.Doors median
#           <int>  <dbl>
#               2    170
#               4    200


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>%
  group_by(Year, Make) %>%
  mutate(spalanko = 1/(city.mpg) * (378.5/1.609)) %>%
  summarize(mean = mean(spalanko, na.rm = TRUE)) %>% 
  arrange(desc(mean)) %>%
  head(5)


# Odp.: 2008 Bugatti i 2009 Bugatti   ok. 29.4 L/100km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?

data %>%
  filter(2007 <= Year & Year <= 2017) %>%
  group_by(Year, Vehicle.Style) %>%
  summarise(pop = sum(Popularity)) %>%
  group_by(Year) %>%
  arrange(desc(pop)) %>%
  slice_head(n = 1)

# Odp.: 
# 1  2007 Extended Cab Pickup  54885
# 2  2008 Crew Cab Pickup     113216
# 3  2009 Crew Cab Pickup     107887
# 4  2010 4dr SUV              78552
# 5  2011 4dr SUV              88240
# 6  2012 4dr Hatchback        55696
# 7  2013 4dr Hatchback        77932
# 8  2014 4dr SUV             144849
# 9  2015 Sedan               986057
# 10  2016 Sedan               998724
# 11  2017 Sedan               776483

# W tabeli najczęściej występuje 4dr SUV i Sedan (jeśli o to chodziło w drugim pytaniu)


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>%
  filter(Year >= 2000, Market.Category == "Luxury,Performance") %>%
  group_by(Make) %>%
  summarise(median = median(MSRP, na.rm = TRUE), mean_cyl = mean(Engine.Cylinders, na.rm = TRUE), mean_HP = mean(Engine.HP, na.rm = TRUE)) %>%
  arrange(desc(median)) %>%
  head(1)

data %>%
  filter(Year >= 2000, Market.Category == "Luxury,Performance") %>%
  group_by(Make) %>%
  summarise(median = median(MSRP, na.rm = TRUE), mean_cyl = mean(Engine.Cylinders, na.rm = TRUE), mean_HP = mean(Engine.HP, na.rm = TRUE)) %>%
  arrange(desc(median)) %>%
  tail(1)

# Odp.: Volkswagen, różnica między średnią liczbą cylindrów to: 10.7 - 6 = 4.7, 
# różnica między średnimi mocy: 397 - 311 = 86 
# (najmniejszą medianę miał Hyundai)


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>%
  filter(Transmission.Type == "AUTOMATIC"& Vehicle.Size == "Midsize") %>%
  mutate(spalanko = 1/(city.mpg) * (378.5/1.609)) %>%
  summarise(quan_01 = quantile(spalanko, 0.1), quan_09 = quantile(spalanko, 0.9))

# # Odp.:   quan_01  quan_09 (spalanie w L/100km)
#          9.409571 16.80281



# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>%
  filter(Make == "Porsche" & Engine.HP <= 300) %>%
  group_by(Model) %>%
  summarise(pop = sum(Popularity)) %>%
  arrange(desc(pop))


# Odp.: 944 / Cayenne


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>%
  group_by(Engine.Fuel.Type, Driven_Wheels) %>%
  summarise(median_HP = median(Engine.HP)) %>%
  arrange(desc(median_HP)) %>%
  head(1)

data %>%
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)" & Driven_Wheels == "all wheel drive") %>%
  mutate(spalanko = 1/(highway.MPG) * (378.5/1.609)) %>%
  summarise(mean_spalanko = mean(spalanko))
  
# Odp.: typ paliwa: flex-fuel (premium unleaded required/E85), 
# rodzaj napędu : all wheel drive 
# średnie spalanie dla tej kombinacji wynosi 12.2638 L/100km

# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>%
  group_by(Make, Model) %>%
  mutate(spalanko_high = 1/(highway.MPG) * (378.5/1.609)) %>%
  mutate(spalanko_city = 1/(city.mpg) * (378.5/1.609)) %>%
  summarise(mean_dif = mean(spalanko_city - spalanko_high)) %>%
  arrange(desc(mean_dif)) %>%
  head(1)

# Odp.: Ferrari Enzo      14.0 L/100km

