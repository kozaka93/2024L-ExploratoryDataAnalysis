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
  summarize(median_hp = median(Engine.HP, na.rm = TRUE))

# Odp.: Dla AUTOMATED_MANUAL mediana wynosi 220
#       Dla AUTOMATIC mediana wynosi 253
#       Dla DIRECT_DRIVE mediana wynosi 147
#       Dla MANUAL mediana wynosi 172
#       Dla UNKNOWN mediana wynosi 125

# Nie wiem na ile ma sens liczenie tej mediany dla rodzaju skrzyni biegów UNKNOWN, 
# żeby tego nie uwzględniać można napisać:

data %>%
  filter(Transmission.Type != "UNKNOWN") %>% 
  group_by(Transmission.Type) %>% 
  summarize(median_hp = median(Engine.HP, na.rm = TRUE))


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>% 
  filter(Make == "Mercedes-Benz", Engine.Fuel.Type == "diesel") %>% 
  summarise(IQR = IQR(city.mpg))
  

# Odp.: 7.25 mpg


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders == 4) %>% 
  group_by(Make) %>% 
  summarise(srednia_cena = mean(MSRP)) %>% 
  arrange(-srednia_cena) %>% 
  head()

# Odp.: "Alfa Romeo", cena = 61600.


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>% 
  filter(Make == "BMW") %>% 
  group_by(Driven_Wheels) %>% 
  summarise(liczba = n()) %>% 
  arrange(-liczba)

data %>% 
  filter(Make == "BMW", Driven_Wheels == "rear wheel drive") %>% 
  group_by(Model) %>% 
  summarise(liczba = n()) %>% 
  arrange(-liczba) %>% 
  head()

# Odp.: Najwiecej z napedem na tył. Najczęstszy model to "1 Sereies"


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make == "Volkswagen") %>%
  group_by(Number.of.Doors) %>% 
  summarise(median_hp = median(Engine.HP, na.rm = TRUE))
  

# Odp.: Dla 2-drzwiowych mediana wynosi 170, dla 4-drzwiowych wynosi ona 200.


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

# Korzystamy z faktu że 1 mpg = 235.21 L/100km

data %>% 
  group_by(Make, Year) %>% 
  mutate(L_100km = 235.21/city.mpg) %>%
  summarise(srednie_spalanie = mean(L_100km, na.rm = TRUE)) %>% 
  arrange(-srednie_spalanie)

# Odp.: Bugatti z 2008 i 2009 roku, spalanie to wynosi 29.4 L/100km dla obu lat.
#       Jeżeli chodziło o to żeby policzyć osobno dla roku i modelu to:

data %>% 
  group_by(Make) %>% 
  mutate(L_100km = 235.21/city.mpg) %>%
  summarise(srednie_spalanie = mean(L_100km, na.rm = TRUE)) %>% 
  arrange(-srednie_spalanie)

data %>% 
  group_by(Year) %>% 
  mutate(L_100km = 235.21/city.mpg) %>%
  summarise(srednie_spalanie = mean(L_100km, na.rm = TRUE)) %>% 
  arrange(-srednie_spalanie)

# Wtedy Odp: Marka - Bugatti (spalanie 29.4L/100km), Rok - 1991 (spalanie 15.4L/100km).


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
data %>% 
  filter(2006 < Year, Year < 2018) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(srednia_pop = mean(Popularity, na.rm = TRUE)) %>% 
  filter(srednia_pop == max(srednia_pop)) %>% 
  select(Year, Vehicle.Style)

data %>% 
  filter(2006 < Year, Year < 2018) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(srednia_pop = mean(Popularity, na.rm = TRUE)) %>% 
  filter(srednia_pop == max(srednia_pop)) %>% 
  select(Year, Vehicle.Style) %>% 
  group_by(Vehicle.Style) %>% 
  summarise(ilosc_wystapien = n()) %>% 
  arrange(-ilosc_wystapien)
  
# Odp.:  Po 3 razy (najwięcej) występowały Cargo Minivan, Cargo Van, Passenger Van:    
#  2007 Cargo Minivan      
#  2008 Crew Cab Pickup    
#  2009 Extended Cab Pickup
#  2010 Extended Cab Pickup
#  2011 Regular Cab Pickup 
#  2012 Cargo Van, Passenger Van (ex aequo)         
#  2013 Cargo Van, Passenger Van         
#  2014 Cargo Van          
#  2015 Cargo Minivan      
#  2016 Cargo Minivan      
#  2017 Passenger Van          


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
  filter(Market.Category == "Luxury,Performance", Year >= 2000) %>% 
  group_by(Make) %>% 
  summarise(mediana_ceny = median(MSRP, na.rm = TRUE)) %>% 
  arrange(-mediana_ceny)

data %>% 
  filter(Market.Category == "Luxury,Performance", Year >= 2000) %>% 
  group_by(Make) %>%
  summarise(srednia_cyl = mean(Engine.Cylinders, na.rm = TRUE), srednia_hp = mean(Engine.HP, na.rm = TRUE)) %>% 
  filter(Make == "Hyundai"| Make == "Volkswagen")

# Odp.: Najwyższą medianę sugerowanej ceny ma Volkswagen. Zakładając że nadal interesują
#       nas tylko samochody wyprodukowane po 2000 roku to dla marki z najwyższą ceną
#       (Volkswagen) średnia moc wynosi 397, a średnia liczba cylindrów 10.7, natomiast 
#       dla marki z najniższą ceną (Hyundai) jest to 311 i 6 odpowiednio.


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>% 
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>% 
  summarise("0.1" = quantile(city.mpg, 0.1),
            "0.9" = quantile(city.mpg, 0.9))

# Odp.: 0.1 kwantyl = 14mpg, 0.9 kwantyl = 25mpg


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>% 
  filter(Make == "Porsche", Engine.HP <= 300) %>% 
  group_by(Model) %>% 
  summarise(srednia_pop = mean(Popularity, na.rm = TRUE)) %>% 
  arrange(-srednia_pop)
  
# Odp.: Niestety wszystkie modele Porsche mają w tej ramce danych taką samą popularność
#       1715 jednostek.


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>% 
  group_by(Engine.Fuel.Type,Driven_Wheels) %>% 
  summarise(median_hp = median(Engine.HP, na.rm = TRUE)) %>% 
  arrange(-median_hp)

data %>% 
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)",
         Driven_Wheels == "all wheel drive") %>% 
  summarise(srednie_spalanie = 235.21/mean(highway.MPG)) 

# Odp.: Kombinacja to: flex-fuel (premium unleaded required/E85), all wheel drive. 
#       Średnie spalanie wynosi 12.2187 L/100km



# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>% 
  group_by(Make, Model) %>% 
  mutate(hwy_l = 235.21/highway.MPG, cty_l = 235.21/city.mpg) %>% 
  summarise(srednie_hwy = mean(hwy_l, na.rm = TRUE), srednie_cty = mean(cty_l, na.rm = TRUE)) %>% 
  mutate(roznica = abs(srednie_hwy - srednie_cty)) %>% 
  arrange(-roznica)

# Odp.: Największa różnica wynosi 14L/100km dla Ferrari Enzo
