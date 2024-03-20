install.packages("dplyr")
library(dplyr)

data <- read.csv("C:/Users/oliwi/Downloads/data.csv")


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


# Odp.:Transmission.Type mediana(w koniach mechanicznych)

       #1 AUTOMATED_MANUAL      220
       #2 AUTOMATIC             253
       #3 DIRECT_DRIVE          147
       #4 MANUAL                172
       #5 UNKNOWN               125



# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>%
  filter(Make == "Mercedes-Benz", Engine.Fuel.Type == "diesel") %>%
  summarise(Rozstep = IQR(city.mpg))


# Odp.: Rozstęp wynosi 7.25.


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders == "4") %>% 
  group_by(Make) %>% 
  summarize(srednia = mean(MSRP, na.rm = TRUE)) %>% 
  arrange(-srednia)

# Odp.: Alfa Romeo, 61600 (jednostka niestety nieznana)


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.

data %>% 
  filter(Make == "BMW") %>% 
  group_by(Driven_Wheels) %>% 
  summarise(n = n()) %>%
  arrange(-n) %>% 
  head(1)

data %>% 
  filter(Make == "BMW" & Driven_Wheels == "rear wheel drive") %>% 
  group_by(Model) %>% 
  summarise(suma = n()) %>% 
  arrange(-suma) %>% 
  head(1)


# Odp.: Z napędem na tylnie koła, "1 series" .


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make == "Volkswagen", Number.of.Doors==2 | Number.of.Doors==4) %>% 
  group_by(Number.of.Doors) %>% 
  summarize(mediana = median(Engine.HP, na.rm = TRUE)) 


# Odp.: Mediana jest większa o 30 dla samochodów 4- drzwiowych(200) niż dla samochodów 2-drzwiowych(170).


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

#będziemy zamieniać z mil/galon na L/100 km, zatem musimy odwrócić mile/galon (aby mieć galony/mile), następnie pomnożyć przez 3.785
#(przybliżony mnożnik z galonów na litry), pomnożyć przez 100(aby uzyskać wynik litrów na 100 km), a potem podzielić przez 1.609 (bo 1 mila == 1.609344 km)
data %>% 
  group_by(Year, Make) %>% 
  mutate(city_new = 1/city.mpg*3.785*100/1.609) %>% 
  summarize(srednia = mean(city_new, na.rm = TRUE)) %>% 
  arrange(-srednia)



# Odp.: Bugatti, z lat 2008 i 2009, to spalanie wynosi 29.4 L/100 km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
data %>% 
  filter(2006 < Year, Year < 2018) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(sr_pop = mean(Popularity, na.rm = TRUE)) %>% 
  filter(sr_pop == max(sr_pop)) %>% 
  select(Year, Vehicle.Style)

data %>% 
  filter(2006 < Year, Year < 2018) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(sr_pop = mean(Popularity, na.rm = TRUE)) %>% 
  filter(sr_pop == max(sr_pop)) %>% 
  select(Year, Vehicle.Style) %>% 
  group_by(Vehicle.Style) %>% 
  summarise(ilosc = n()) %>% 
  arrange(-ilosc)

# Odp.: średnio najbardziej popularne w kolejnych latach:
#  2007 Cargo Minivan      
#  2008 Crew Cab Pickup    
#  2009 Extended Cab Pickup
#  2010 Extended Cab Pickup
#  2011 Regular Cab Pickup 
#  2012 Cargo Van, Passenger Van      
#  2013 Cargo Van, Passenger Van      
#  2014 Cargo Van          
#  2015 Cargo Minivan      
#  2016 Cargo Minivan      
#  2017 Passenger Van    
# Jako średnio najbardziej popularne przez te 10 lat wystąpiły 3 style: Cargo Minivan, Cargo Van oraz Passenger Van.


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
  filter(Market.Category == "Luxury,Performance", Year >= 2000) %>% 
  group_by(Make) %>% 
  summarise(prize_med = median(MSRP, na.rm = TRUE)) %>% 
  arrange(-prize_med) %>% 
  head(1)

data %>% 
  filter(Market.Category == "Luxury,Performance", Year >= 2000) %>% 
  group_by(Make) %>%
  summarise(mean_cyl = mean(Engine.Cylinders, na.rm = TRUE), mean_hp = mean(Engine.HP, na.rm = TRUE)) %>% 
  filter(Make == "Hyundai"| Make == "Volkswagen")

# Odp.: Najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance" mają samochody marki Volkswagen(94600)
# Różnice: liczby cylindrów: 10.7(Volkswagen, najwyższa mediana sug. ceny detalicznej),  6 (Hyundai, najniższa mediana ceny sug. ceny det.)
#mocy silnika: 397(Volkswagen), 311(Hyundai).

# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>% 
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>% 
  summarise("0.1 quantile" = quantile(city.mpg, 0.1),
            "0.9 quantile" = quantile(city.mpg, 0.9))


# Odp.: Kwantyl 0.1 wynosi 14 mpg, a 0.9 kwantyl - 25 mpg.


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>% 
  filter(Make == "Porsche", Engine.HP <= 300) %>% 
  group_by(Model) %>% 
  summarise(sr_pop = mean(Popularity, na.rm = TRUE)) %>% 
  arrange(-sr_pop) 

# Odp.: Brak drugiego najbardziej popularnego modelu marki Porsche, wszystkie mają równą średnią popularność
        # wynoszącą 1715.


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>% 
  group_by(Engine.Fuel.Type,Driven_Wheels) %>% 
  summarise(med_hp = median(Engine.HP, na.rm = TRUE)) %>% 
  arrange(-med_hp) %>% 
  head(1)

data %>% 
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)",
         Driven_Wheels == "all wheel drive") %>% 
         mutate(highway_lp100km = 1/highway.MPG*3.785*100/1.609) %>% 
         summarise(sr_spalanie = mean(highway_lp100km))


# Odp.: Szukana kombinacja to flex-fuel (premium unleaded required/E85), all wheel drive. 
#       Średnie spalanie wynosi 12.2638 L/100km (wynik może się troche różnić przez zaokrąglenie różnic jednostek)


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

# Tu dla ułatwienia obliczeń i kodu przyjmujemy, że 1 mpg = 235.21 L/100km.
data %>% 
  group_by(Make, Model) %>% 
  mutate(hwy_l = 235.21/highway.MPG, cty_l = 235.21/city.mpg) %>% 
  summarise(sr_hwy = mean(hwy_l, na.rm = TRUE), sr_cty = mean(cty_l, na.rm = TRUE)) %>% 
  mutate(roz = abs(sr_hwy - sr_cty)) %>% 
  arrange(-roz) %>% 
  head(1)

# Odp.: Kombinacja marki i modelu samochodu z największą średnią różnicą
# spalania w mieście i na autostradzie w L/100 km to Ferrari Enzo, różnica ta wynosi 14L/100km.

