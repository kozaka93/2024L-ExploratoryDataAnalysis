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
  summarise(Mediana = median(Engine.HP, na.rm = TRUE))

# Odp.:
#1 AUTOMATED_MANUAL               220
#2 AUTOMATIC                      253
#3 DIRECT_DRIVE                   147
#4 MANUAL                         172
#5 UNKNOWN                        125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>%
  filter(Make == "Mercedes-Benz", Engine.Fuel.Type == "diesel") %>%
  summarise(kwantyl = IQR(city.mpg))

# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>%
  filter(Engine.Cylinders == 4) %>%
  group_by(Make) %>%
  summarise(srednia = mean(MSRP, na.rm = TRUE)) %>%
  arrange(-srednia) %>%
  top_n(1, srednia)

# Odp.: Alfa Romeo        61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
# najpopularniejszy rodzaj napędu wśród BMW
data %>%
  filter(Make == "BMW") %>%
  group_by(Driven_Wheels) %>% 
  summarise(NN = n()) %>% 
  arrange(-NN)%>% 
  head(1)

# najczęściej występujący model dla tego napędu
data %>%
  filter(Make == "BMW", Driven_Wheels  == "rear wheel drive") %>% 
  group_by(Model) %>% 
  summarise(NN = n()) %>% 
  arrange(-NN) %>% 
  head(1)

# Odp.: rear wheel drive (czyli tylne), 1 Series 


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>%
  filter(Make == "Volkswagen") %>%
  group_by(Number.of.Doors) %>%
  summarise(Mediana = median(Engine.HP, na.rm = TRUE))

# Odp.: dla 2 drzwi - 170, dla 4 drzwi - 200


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>%
  group_by(Year, Make) %>%
  summarise(srednia = mean(city.mpg, na.rm = TRUE)) %>%
  mutate(spalanie = (235.215 / srednia)) %>% 
  arrange(-spalanie) %>%
  top_n(1, spalanie)


# Odp.:   Rok   Marka             srednia    srednie spalanie L/100km
      #1  2008 Bugatti                  8                        29.4
      #2  2009 Bugatti                  8                        29.4
      #3  2001 Ferrari                  8.5                      27.7


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
data %>%
  filter(Year >= 2007, Year <= 2017) %>%
  group_by(Year, Vehicle.Style) %>% 
  summarise(pop = mean(Popularity)) %>% 
  top_n(1, pop) -> idk

idk

idk %>%
  ungroup() %>%
  group_by(Vehicle.Style) %>% 
  summarise(NN = n()) %>% 
  arrange(-NN) %>%
  top_n(1, NN)

# Odp.: 

# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Year Vehicle.Style         popularnosc
# 1  2007 Cargo Minivan       2964.
# 2  2008 Crew Cab Pickup     2461.
# 3  2009 Extended Cab Pickup 3583.
# 4  2010 Extended Cab Pickup 3093 
# 5  2011 Regular Cab Pickup  5657 
# 6  2012 Cargo Van           5657 
# 7  2012 Passenger Van       5657 
# 8  2013 Cargo Van           5657 
# 9  2013 Passenger Van       5657 
# 10  2014 Cargo Van           2858.
# 11  2015 Cargo Minivan       4337 
# 12  2016 Cargo Minivan       4051.
# 13  2017 Passenger Van       5657 

# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
# Vehicle.Style      NN
# 1 Cargo Minivan     3
# 2 Cargo Van         3
# 3 Passenger Van     3



# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>%
  filter(Year >= 2000, Market.Category == "Luxury,Performance") %>%
  group_by(Make) %>%
  summarise(Mediana = median(MSRP, na.rm = TRUE)) %>%
  arrange(-Mediana) %>% View

# Odp.: Najwysza mediane ma Volkswagen  94600

data %>%
  filter(Year >= 2000, Market.Category == "Luxury,Performance") %>%
  group_by(Make) %>%
  summarise(Mediana = median(MSRP, na.rm = TRUE),
            srednia_cylindry = mean(Engine.Cylinders, na.rm = TRUE),
            srednia_konie = mean(Engine.HP, na.rm = TRUE)) %>%
  arrange(-Mediana) %>% 
  select(Make, Mediana, srednia_cylindry, srednia_konie) -> cos

cos %>% head(1)
cos %>% arrange(Mediana) %>% head(1)


#najwyzsza: 
# Make           Mediana srednia_cylindry srednia_konie
# 1 Volkswagen   94600               10.7           397

#najnizsza:
# Make      Mediana srednia_cylindry srednia_konie
# 1 Hyundai   39625                6           311

#róznica to 4,7 w cylidrach i 86 koni mechanicznych. 


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>%
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>%
  summarise(
    K_0.1= quantile(city.mpg, 0.1, na.rm = TRUE),
    K_0.9 = quantile(city.mpg, 0.9, na.rm = TRUE)
  )

# Odp.:  kwantyl 0.1 to 14, a 0.9 kwantyl to 25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>%
  filter(Make == "Porsche", Engine.HP <= 300) %>%
  group_by(Model)%>% 
  summarise(NN = n()) %>%
  arrange(-NN) %>%
  head(2)

# Odp.: 944 


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>%
  group_by(Engine.Fuel.Type, Driven_Wheels) %>%
  summarise(
    Mediana = median(Engine.HP, na.rm = TRUE),
    srednia = mean(highway.MPG, na.rm = TRUE)
  ) %>%
  arrange(-Mediana) %>%
  mutate(spalanie = (235.215 / srednia)) %>%
  head(1)


# Odp.:flex-fuel (premium unleaded required/E85), 	all wheel drive
# spalanie 12.21896 L/100km

# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>%
  mutate(
    miasto = 235.215 / city.mpg,
    autostrada = 235.215 / highway.MPG) %>%
  group_by(Make, Model) %>%
  mutate(roznica = mean( abs(miasto - autostrada))) %>%
  arrange(-roznica) %>%
  head(1) %>% select(Make, Model, roznica)

# Odp.:  

# Marka       Model   roznica
# Ferrari     Enzo      14.0

