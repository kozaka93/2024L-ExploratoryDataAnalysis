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
#Transmission.Type mediana
#<chr>               <dbl>
#1 AUTOMATED_MANUAL      220
#2 AUTOMATIC             253
#3 DIRECT_DRIVE          147
#4 MANUAL                172
#5 UNKNOWN               125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>% 
  filter(Make == "Mercedes-Benz" & Engine.Fuel.Type == "diesel") %>% 
  summarise(Interquartile_Range = IQR(city.mpg))

# Odp.:   7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders == 4) %>% 
  group_by(Make) %>% 
  summarise(mean_price = mean(MSRP)) %>% 
  arrange(-mean_price) %>% 
  head(1)

# Odp.: Producent: Alfa Romeo, cena: 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>% 
  filter(Make == "BMW") %>% 
  group_by(Driven_Wheels) %>% 
  summarise(number_of_cars = n()) %>% 
  arrange(-number_of_cars)

#Driven_Wheels     number_of_cars
#<chr>                      <int>
#1 rear wheel drive             189
#2 all wheel drive              144
#3 front wheel drive              1

data %>% 
  filter(Driven_Wheels =="rear wheel drive" & Make == "BMW") %>% 
  group_by(Model) %>% 
  summarise(number_of_models = n()) %>% 
  arrange(-number_of_models) %>% 
  head()

#Model    number_of_models
#<chr>               <int>
#1 1 Series               16
#2 3 Series               15
#3 4 Series               15
#4 6 Series               12
#5 2 Series               10
#6 7 Series                9

# Odp.: Najwięcej jest aut BMW z napedem na tył, najczęściej występujący 
# model dla tego rodzaju napędu to: 1 Series.

# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(!is.na(Number.of.Doors) & Make == "Volkswagen") %>% 
  group_by(Number.of.Doors) %>% 
  summarise(mediana = median(Engine.HP))
#Number.of.Doors mediana
#<int>   <dbl>
#1               2     170
#2               3      90
#3               4     200

# Odp.: Dla samochodów posiadających 2 drzwi mediana wynosi 170, dla samochodów 
# z 4 drzwiami mediana wynosi 200.

# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.


data %>% 
  group_by(Year, Make) %>% 
  summarise(mean_mileage = mean((100 * 3.785411784) / (city.mpg * 1.609344))) %>% 
  arrange(-mean_mileage) %>% 
  head(5)

#Year Make        mean_mileage
#<int> <chr>              <dbl>
#1  2008 Bugatti             29.4
#2  2009 Bugatti             29.4
#3  2001 Ferrari             27.8
#4  2009 Lamborghini         27.8
#5  2008 Lamborghini         27.4

# Odp.: Największe średnie spalanie w mieście mają samochody marki Bogatti z 2008
#i 2009 roku, wynosiło ono 29.4 L/100km.


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
df1 <- data %>% 
  filter(Year>= 2007 & Year <= 2017) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(mean_popularity = mean(Popularity)) %>% 
  arrange(-mean_popularity)

df2 <- df1 %>% 
  group_by(Year) %>% 
  summarise(max_popularity = max(mean_popularity))

df3 <- df1 %>% 
  left_join(df2, by = "Year") %>% 
  filter(mean_popularity == max_popularity) %>% 
  arrange(Year) %>% 
  select(Year, Vehicle.Style, mean_popularity)
df3

df3 %>% 
  group_by(Vehicle.Style ) %>% 
  summarise(popularity2 = n()) %>% 
  arrange(-popularity2)
  

# Odp.:
#Year Vehicle.Style       mean_popularity
#<int> <chr>                         <dbl>
#1  2007 Cargo Minivan                 2964.
#2  2008 Crew Cab Pickup               2461.
#3  2009 Extended Cab Pickup           3583.
#4  2010 Extended Cab Pickup           3093 
#5  2011 Regular Cab Pickup            5657 
#6  2012 Cargo Van                     5657 
#7  2012 Passenger Van                 5657 
#8  2013 Cargo Van                     5657 
#9  2013 Passenger Van                 5657 
#10  2014 Cargo Van                     2858.
#11  2015 Cargo Minivan                 4337 
#12  2016 Cargo Minivan                 4051.
#13  2017 Passenger Van                 5657 

# W latach 2007-2017 najczęściej średnio najpopularniejszym 
# stylem był: Cargo Minivan, Cargo Van, Passenger Van.

# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
  filter(Year >= 2000, Market.Category =="Luxury,Performance") %>% 
  group_by(Make) %>% 
  summarise(mediana = median(MSRP)) %>% 
  arrange(-mediana) 

data %>% 
  filter(Year >= 2000 & Market.Category =="Luxury,Performance" & Make %in% c("Volkswagen", "Hyundai")) %>% 
  group_by(Make) %>% 
  summarise(mean_Engine.Cylinders = mean(Engine.Cylinders), mean_Engine.HP = mean(Engine.HP))

#Make       mean_Engine.Cylinders mean_Engine.HP
#<chr>                      <dbl>          <dbl>
#1 Hyundai                      6              311
#2 Volkswagen                  10.7            397

# Odp.: Najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance" mają 
# samochody marki Volkswagen. Najniższą medianę sugerowanej ceny w kategorii 
# "Luxury,Performance" mają samochody marki Hyundai. Srednia liczba cylindrów 
# dla samochodów marki Hyundai w tej kategorii wynosi 6, zaś dla samochodów
# marki Volkswagen wynosi ona 10.7 (czyli różni się o 4.7). Średnia liczba koni mechanicznych silnika 
# dla samochodów marki Hyundai w tej kategorii wynosi 311, zaś dla samochodów 
# marki Volkswagen wynosi 397 (czyli różni się 86).


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>% 
  filter(Vehicle.Size == "Midsize" & Transmission.Type == "AUTOMATIC") %>% 
  summarise(quantiles = quantile(city.mpg, probs = c(0.1, 0.9)))

#quantiles
#1        14
#2        25

# Odp.: 0.1 kwantyl wynosi 14, 0.9 kwanty wynosi 25.

# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

# Porównuję maksymalną popularnośc dla każdego modelu
data %>% 
  filter(Make == "Porsche", Engine.HP <= 300) %>% 
  group_by(Model) %>% 
  summarise(max_popularity = max(Popularity)) %>% 
  arrange(-max_popularity) %>% 
  head()

#Model      mean_popularity
#<chr>                <dbl>
#1 718 Cayman            1715
#2 944                   1715
#3 968                   1715
#4 Boxster               1715
#5 Cayenne               1715
#6 Cayman                1715

# Odp.: Wszystkie modele marki Porsche o liczbie koni mechanicznych <= 300
# są tak samo popularne.


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  summarise(mediana = median(Engine.HP)) %>% 
  arrange(-mediana)

#Engine.Fuel.Type                             Driven_Wheels    mediana
#<chr>                                        <chr>              <dbl>
#1 flex-fuel (premium unleaded required/E85)    all wheel drive     608 
#2 flex-fuel (premium unleaded required/E85)    four wheel drive    510 
#3 premium unleaded (recommended)               four wheel drive    420 
#4 flex-fuel (premium unleaded recommended/E85) all wheel drive     403 

data %>% 
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)" &
           Driven_Wheels == "all wheel drive") %>% 
  summarise(mean_highway = mean(100 * 3.78541178 /(1.609344 * highway.MPG)))

#mean_highway
#1     12.26251

# Odp.: Mediana koni mechanicznych jest największa dla kombinacji typ paliwa:
# flex-fuel (premium unleaded required/E85), rodzaj napędu: all wheel drive, 
# średnie spalanie na autostradzie dla tej kombinacji wynosi 12.26251 L/100km


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>% 
  group_by(Make, Model) %>% 
  summarise(mean_difference = abs(100 * 3.78541178 /1.609344 * (1/highway.MPG - 1/city.mpg))) %>% 
  arrange(-mean_difference)

#Make    Model          mean_difference
#<chr>   <chr>                    <dbl>
#1 Ferrari Enzo                      14.0
#2 Bugatti Veyron 16.4               12.6
#3 Bugatti Veyron 16.4               12.6
#4 Bugatti Veyron 16.4               12.6
#5 Ferrari 575M                      11.4

# Odp.: Największą średnią różnicę pod tym względem ma Ferrari Enzo, wynosi ona 14.0 L/100km

