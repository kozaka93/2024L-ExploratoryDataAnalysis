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
# 1 AUTOMATED_MANUAL      220
# 2 AUTOMATIC             253
# 3 DIRECT_DRIVE          147
# 4 MANUAL                172
# 5 UNKNOWN               125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?
zad_2 <- data %>% 
  filter(Make == "Mercedes-Benz", Engine.Fuel.Type == "diesel")
  
IQR(zad_2$city.mpg)


# Odp.: 7.25 mila/galon


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders == 4) %>% 
  group_by(Make) %>% 
  summarize(srednia = mean(MSRP)) %>% 
  arrange(-srednia)


# Odp.: Alfa Romeo 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>% 
  filter(Make == "BMW") %>% 
  group_by(Driven_Wheels) %>% 
  summarize(n = n()) %>% 
  arrange(-n)

data %>% 
  filter(Driven_Wheels == "rear wheel drive", Make == "BMW") %>% 
  group_by(Model) %>% 
  summarize(n = n()) %>% 
  arrange(-n)

# Odp.: najwięcej z napędem na 4 koła, 1 Series


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make == "Volkswagen") %>% 
  group_by(Number.of.Doors) %>% 
  summarize(mediana = median(Engine.HP))

# Odp.: Dla 4 drzwi jest o 30 koni większa niż dla dwóch 


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>% 
  group_by(Year, Make) %>% 
  summarize(srednie_spalanie = (1 / mean(city.mpg))*235.22) %>% 
  arrange(-srednie_spalanie)

# Odp.: 2008 Bugatti 29.4 L/100 km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
zad_7 <- data %>% 
  filter(2007 <= Year, Year <= 2017) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarize(srednia_popularnosc = mean(Popularity)) %>% 
  group_by(Year) %>% 
  top_n(1, srednia_popularnosc) 

zad_7$Vehicle.Style

zad_7 %>% 
  group_by(Vehicle.Style) %>% 
  summarize(liczba_wystapien = n()) %>% 
  arrange(-liczba_wystapien)

# Odp.: Nie wiem czy dobrze zrozumiałem polecenie ale w kolejnych latach począwszy od 2007 najbardziej popularne style to:
# "Cargo Minivan"       "Crew Cab Pickup"     "Extended Cab Pickup" "Extended Cab Pickup" "Regular Cab Pickup"  "Cargo Van"          
# "Passenger Van"       "Cargo Van"           "Passenger Van"       "Cargo Van"           "Cargo Minivan"       "Cargo Minivan"      
# "Passenger Van".
# Wśród nich najczęściej (po 3 razy) występują Cargo Minivan, Cargo Van i Passenger Van


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

zad_8 <- data %>% 
  filter(Market.Category =="Luxury,Performance", Year >= 2000) %>% 
  group_by(Make) %>% 
  summarize(mediana = median(MSRP), srednia_liczba_cylindrow = mean(Engine.Cylinders), srednia_moc = mean(Engine.HP)) %>% 
  arrange(-mediana)

zad_8[c(1, length(zad_8[[1]])),]



# Odp.: Volkswagen, średnia cylindrów jest większa dla Volkswagena o 4.7 cylindra, i ma większą średnią moc od Hyundaia o 86 HP


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

zad_9 <- data %>% 
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize")
  
quantile(zad_9$city.mpg, probs=c(0.1, 0.9))

# Odp.: 14, 25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

zad_10 <- data %>% 
  filter(Make =="Porsche", Engine.HP <= 300) %>% 
  group_by(Model) %>% 
  summarize(popularnosc = sum(Popularity)) %>% 
  arrange(-popularnosc) 

zad_10[2,]

# Odp.: Wszystkie te modele (rozróżniając roczniki) miały taką samą popularność więc wygrało to co miało najwięcej
# różnych roczników. Na drugim miejscu są wtedy 2 modele: 944 i Cayenne


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  summarize(mediana_HP = median(Engine.HP), srednie_spalanie = (1 / mean(highway.MPG))*235.22) %>% 
  arrange(-mediana_HP)


# Odp.: flex-fuel (premium unleaded required/E85), all wheel drive, spalanie - 12.2


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>% 
  group_by(Make, Model) %>% 
  summarize(srednia_miasto = (1 / mean(city.mpg)*235.22), srednia_autostrada = (1 / mean(highway.MPG)*235.22)) %>% 
  mutate(roznica = srednia_miasto - srednia_autostrada) %>% 
  arrange(-roznica)


# Odp.: Ferrari Enzo 14 L/100 km

