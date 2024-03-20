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
  filter(Make == "Mercedes-Benz", Engine.Fuel.Type == "diesel") %>%
  summarise(rmk = IQR(city.mpg, na.rm = TRUE))

# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?
 
data %>%
  filter(Engine.Cylinders == "4") %>%
  group_by(Make) %>%
  summarise(srednia = mean(MSRP, na.rm = TRUE)) %>%
  top_n(1, srednia)



# Odp.: Alfa Romeo - 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.

data %>%
  filter(Make == "BMW") %>%
  group_by(Driven_Wheels) %>%
  summarise(n = n()) %>%
  top_n(1,n)

data %>%
  filter(Make == "BMW", Driven_Wheels == "rear wheel drive") %>%
  group_by(Model) %>%
  summarise(l = n()) %>%
  arrange(-l) %>%
  head(1)


# Odp.: napęd na tył, 1 Series


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>%
  filter(Make == "Volkswagen") %>%
  group_by(Number.of.Doors) %>%
  summarise(mediana = median(Engine.HP, na.rm = TRUE))


# Odp.: Dla 2 drzwiowych mediana wynosi 170, dla 4 drzwiowych wynosi 200


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>% 
  group_by(Make, Year) %>% 
  summarise(srednia = mean(city.mpg, na.rm = TRUE)) %>% 
  mutate(srednia_w_km = 100/(srednia/3.78541178 * 1.609344)) %>% 
  arrange(-srednia_w_km) 


# Odp.: 2008/2009, Bugatti, Spalanie: około 29.4 L/100km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
data %>%
  filter(Year <= 2017 & Year >= 2007) %>%
  group_by(Year,Vehicle.Style) %>% 
  summarize(srednia = mean(Popularity)) %>% 
  filter(srednia == max(srednia))

data %>%   
  filter(Year>=2007& Year<=2017) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarize(srednia = mean(Popularity)) %>% 
  filter(srednia == max(srednia)) %>% 
  group_by(Vehicle.Style) %>% 
  summarize(n = n()) %>% 
  arrange(-n)

# Odp.:  
#Year Vehicle.Style       srednia
# <int> <chr>                 <dbl>
#   1  2007 Cargo Minivan         2964.
# 2  2008 Crew Cab Pickup       2461.
# 3  2009 Extended Cab Pickup   3583.
# 4  2010 Extended Cab Pickup   3093 
# 5  2011 Regular Cab Pickup    5657 
# 6  2012 Cargo Van             5657 
# 7  2012 Passenger Van         5657 
# 8  2013 Cargo Van             5657 
# 9  2013 Passenger Van         5657 
# 10  2014 Cargo Van             2858.
# 11  2015 Cargo Minivan         4337 
# 12  2016 Cargo Minivan         4051.
# 13  2017 Passenger Van         5657

# Najpopularniejszy styl to Cargo Minivan, Cargo Van, Passenger Van

# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>%
  filter(Market.Category == "Luxury,Performance", Year >= 2000) %>%
  group_by(Make) %>%
  summarise(mediana = median(MSRP)) %>%
  arrange(-mediana)
  
  
data %>%
  filter(Market.Category == "Luxury,Performance", Year >= 2000) %>%
  filter(Make == "Volkswagen" | Make == "Hyundai") %>%
  group_by(Make) %>%
  summarise(c = mean(Engine.Cylinders,na.rm = TRUE), s = mean(Engine.HP,na.rm = TRUE))


# Odp.: Volkswagen. Dla samochodów marki z najwyższą medianą: średnia liczba cylindrów to 10.7 i średnia moc silnika to 397.
# Dla samochodów marki z najniższą medianą: średnia liczba cylindrów to 6 i średnia moc silnika to 311.


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?
data %>%
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>%
  summarise(kw1 = quantile(city.mpg,0.1), kw2 = quantile(city.mpg,0.9))


# Odp.: 0.1 kwantyl : 14, 0.9 kwantyl: 25. Odpowiedz w jednostce mil/galon.


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

# Z powodu takiego, że dla każdego samochodu marki Porsche "Popularity" wynosi tyle samo, 
# korzystam z funkcji n() aby znaleźć odpowiedź na to zadanie.

data %>% 
  filter(Make == "Porsche", Engine.HP <= 300) %>% 
  group_by(Model) %>% 
  summarise(n=n()) %>% 
  arrange(-n)
  

# Odp.:Model 944 i Cayenne


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>%
  group_by(Engine.Fuel.Type,Driven_Wheels) %>%
  summarise(mediana = median(Engine.HP, na.rm = TRUE)) %>%
  arrange(-mediana) %>%
  head(1)

data %>%
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)", Driven_Wheels == "all wheel drive") %>%
  summarise(srednia = 100/(mean(highway.MPG)/3.78541178 * 1.609344))

# Odp.: flex-fuel (premium unleaded required/E85) i all wheel drive, około 12,22L/100km


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>%
  group_by(Make,Model) %>%
  summarise(autostrada = 100/(mean(highway.MPG)/3.78541178 * 1.609344),miasto = 100/(mean(city.mpg)/3.78541178 * 1.609344)) %>%
  mutate(roznica = abs(miasto - autostrada)) %>%
  arrange(-roznica) %>%
  head(1)

# Odp.: Ferrari, Enzo. Różnica: 14 L/100km

