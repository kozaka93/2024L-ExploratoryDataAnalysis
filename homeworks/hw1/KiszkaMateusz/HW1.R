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


# Odp.:
#Transmission.Type      median_hp
#<chr>                    <dbl>
#1 AUTOMATED_MANUAL        220
#2 AUTOMATIC               253
#3 DIRECT_DRIVE            147
#4 MANUAL                  172
#5 UNKNOWN                 125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?
x <- data %>% 
    filter(Make == "Mercedes-Benz", Engine.Fuel.Type == "diesel") %>% 
    .$city.mpg
IQR(x)

# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?
data %>% 
    filter(Engine.Cylinders == "4") %>% 
    group_by(Make) %>% 
    summarise(Mean_MSRP = mean(MSRP)) %>% 
    arrange(desc(Mean_MSRP))



# Odp.:
# Alfa Romeo wynosi ona 61600 

# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>% 
    filter(Make == "BMW") %>% 
    count(Driven_Wheels) %>% 
    arrange(desc(n))
data %>% 
    filter(Make == "BMW", Driven_Wheels == "rear wheel drive") %>% 
    count(Model) %>% 
    arrange(desc(n))

# Odp.:
# Najwięcej aut BMW ma napęd na tylne koła. Najczęściej występującym modelem z napędem na tylne koła jest 1 series. 


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?
data %>% 
    filter(Make == "Volkswagen") %>%
    group_by( Number.of.Doors) %>% 
    summarise(median_hp = median(Engine.HP, na.rm = TRUE))

# Odp.: 
# Number.of.Doors    median_hp
#               2       170
#               3        90
#               4       200

# Dla 2 drzwiowych aut tej marki mediana mocy wynosi 170 km, natomiast dla 4 drzwiowych - 200 km


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>% 
    mutate(city_cons_l_100km = 235.21 / city.mpg) %>%  #Wygodny przelicznik z mpg na l/100km
    group_by(Make, Year) %>% 
    summarise(mean_city_cons_l_100km = mean(city_cons_l_100km)) %>% 
    arrange(desc(mean_city_cons_l_100km)) %>% 
    head(1)
    

# Odp.:
#  Jest to Bugatti z 2008 roku ze spalaniem 29.4 l/100km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
x <- data %>% 
    filter(Year >= 2007 , Year <= 2017) %>% 
    group_by(Year, Vehicle.Style) %>% 
    summarise(mean_pop = mean(Popularity)) %>% 
    group_by(Year) %>% 
    filter(mean_pop == max(mean_pop)) %>% 
    arrange(desc(Vehicle.Style))
x
x %>%
    group_by(Vehicle.Style) %>% 
    summarise(freq = n()) %>% 
    arrange(desc(freq))

# Odp.:
# Średnio najbardziej popularne były: 
# 2011 Regular Cab Pickup, 2012 Passenger Van, 2013 Passenger Van, 2017 Passenger Van          

# Najczęsciej jako najbardziej popularny wystąpiły: 
# Cargo Minivan, Cargo Van, Passenger Van

# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
    filter(Market.Category == "Luxury,Performance" , Year >= 2000) %>% 
    group_by(Make) %>% 
    summarise(median_MSRP = median(MSRP)) %>% 
    arrange(desc(median_MSRP))

data %>% 
    filter(Market.Category == "Luxury,Performance" , Year >= 2000) %>% 
    group_by(Make) %>% 
    summarise(median_MSRP = median(MSRP)) %>% 
    arrange(median_MSRP)

data %>% 
    filter(Make %in% c("Volkswagen", "Hyundai"), Market.Category == "Luxury,Performance" , Year >= 2000) %>%
    group_by(Make) %>% 
    summarise(mean_cyl = mean(Engine.Cylinders, na.rm=TRUE), mean_hp = mean(Engine.HP, na.rm = TRUE))
# Odp.:
# Najwyższą medianę sugerowanej ceny w tej kategorii mają samochody Volkswagena.
#   Make       mean_cyl mean_hp
#   <chr>         <dbl>   <dbl>
# Hyundai         6       311
# Volkswagen     10.7     397


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

q <- data %>% 
    filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>% 
    .$city.mpg

quantile(q, 0.1)
quantile(q, 0.9)
# Odp.:
#0.1 kwantyl wynosi 14
#0.9 kwantyl wynosi 25

# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

pop <- data %>% 
    filter(Make == "Porsche", Engine.HP <= 300) %>% 
    .$Popularity
unique(pop)    

data %>% 
    filter(Make == "Porsche", Engine.HP <= 300) %>% 
    group_by(Model) %>% 
    summarise(num_models = n()) %>% 
    arrange(desc(num_models))
# Odp.:
# Wszystkie modele są równie popularne, więc nie można mówić o drugim najpopularniejszym. (względem popularity)
# Natomiast jeżeli za najpopularniejszy przyjmiemy ten którego modeli było najwięcej to będzię to:

#   Model           num_models
# 1 968                 6
# 2 944                 4
# 3 Cayenne             4
# 4 Boxster             3
# 5 Cayman              3
# 6 718 Cayman          1
# 7 Cayman S            1
# 8 Macan               1

# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
data %>% 
    group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
    summarise(median_hp = median(Engine.HP, na.rm = TRUE)) %>% 
    arrange(desc(median_hp))

data %>% 
    mutate(highw_cons_l_100km = 235.21 / highway.MPG) %>% 
    filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)", Driven_Wheels == "all wheel drive") %>% 
    summarise(mean_highw_cons_l_100km = mean(highw_cons_l_100km))

# Odp.:
# Mediana mocy jest największa dla kombinacji paliwa flex-fuel (premium unleaded required/E85) i napędu all wheel drive  
# i wynosi 608 km. 

# średnie spalanie na autostradzie wynosi dla tej kombinacji 12.26227 l/100km

# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?
data %>% 
    mutate(highw_cons_l_100km = 235.21 / highway.MPG, city_cons_l_100km = 235.21 / city.mpg) %>%
    group_by(Make, Model) %>% 
    summarise(dif = abs(mean(city_cons_l_100km) - mean(highw_cons_l_100km))) %>% 
    arrange(desc(dif))


# Odp.:
# Ferrari Enzo, różnica wynosi 14.0 l/100km 