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
  summarise(mediana = median(Engine.HP, na.rm = TRUE)) %>% 
  select(Transmission.Type, mediana)

# Odp.:
#1 AUTOMATED_MANUAL      220
#2 AUTOMATIC             253
#3 DIRECT_DRIVE          147
#4 MANUAL                172
#5 UNKNOWN               125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?
data_2 <- data %>% 
  filter(Make == "Mercedes-Benz", Engine.Fuel.Type == "diesel")
answer_2 <- IQR(data_2$city.mpg, na.rm = TRUE)
answer_2
# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?
data %>% 
  filter(Engine.Cylinders == 4) %>% 
  group_by(Make) %>% 
  summarise(średnia = mean(MSRP)) %>% 
  arrange(-średnia)


# Odp.:Alfa Romeo     61600 


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  data %>% 
  filter(Make == "BMW") %>% 
  group_by(Driven_Wheels) %>% 
  summarise(suma = n())
  
data %>% 
  filter(Make == "BMW", Driven_Wheels == "rear wheel drive") %>% 
  group_by(Model) %>% 
  summarise(liczba = n()) %>% 
  arrange(-liczba)

# Odp.:   Driven_Wheels      suma
#1      all wheel drive       144
#2    front wheel drive        1
#3     rear wheel drive       189

#b) 1 Series


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?
data %>% 
  filter(Make == "Volkswagen") %>% 
  filter(Number.of.Doors == 2 | Number.of.Doors == 4) %>% 
  group_by(Number.of.Doors) %>% 
  summarise(mediana = median(Engine.HP, na.rm = TRUE))


# Odp.:dla 4 drzwiowych
#Number.of.Doors mediana
     #2            170
     #4            200


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.
data %>% 
  mutate(spalanie_l_100_km = 235.21/city.mpg) %>%  #znalazłem w internecie, że tak zamienia się tę jednostkę
  group_by(Make, Year) %>% 
  summarise(średnie_spalanie = mean(spalanie_l_100_km, na.rm = TRUE)) %>% 
  arrange(-średnie_spalanie)


# Odp.:
#Make           Year        średnie_spalanie
#1 Bugatti      2008             29.4
#2 Bugatti      2009             29.4

# Zadanie 7 --------------------------------------------------------------- 
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?

#chciałbym napisać, że nie jestem pewny jak interpretować to polecenie, wydaje mi się do końca niejasne
#czy powinienem skorzystać z kolumny Popularity czy zliczyć występowanie danego stylu samochodu,
#ale skorzystałem z tej pierwszej opcji

lata <- data %>% 
  filter(Year >= 2007, Year <= 2017) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarize(Average_Popular = mean(Popularity)) %>% 
  group_by(Year) %>% 
  filter(Average_Popular == max(Average_Popular))
View(lata)

  lata %>% 
    group_by(Vehicle.Style) %>% 
    summarise(sumka = n()) %>% 
    arrange(-sumka)
 

# Odp.:w latach 2007-2017:
#a)
#2007 Cargo Minivan                
#2008 Crew Cab Pickup        
#2009 Extended Cab Pickup        
#2010 Extended Cab Pickup         
#2011 Regular Cab Pickup            
#2012 Cargo Van, Passenger Van            
#2013 Passenger Van                   
#2013 Cargo Van, Passenger Van                
#2014 Cargo Van                   
#2015 Cargo Minivan               
#2016 Cargo Minivan               
#2017 Passenger Van               
#b)
#najczesciej Passenger Van, Cargo Van i Cargo Minivan

# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?
data %>% 
  filter(Market.Category == "Luxury,Performance", Year >= 2000) %>% 
  group_by(Make) %>% 
  summarise(mediana_ceny = median(MSRP, na.rm = TRUE)) %>% 
  arrange(mediana_ceny)

data %>% 
  filter(Make == "Volkswagen", Market.Category == "Luxury,Performance") %>% 
  group_by(Make) %>% 
  summarise(avg_cylin = mean(Engine.Cylinders, na.rm = TRUE),
            avg_power = mean(Engine.HP, na.rm = TRUE))

data %>% 
  filter(Make == "Hyundai", Market.Category == "Luxury,Performance") %>%
  group_by(Make) %>% 
  summarise(avg_cylin = mean(Engine.Cylinders, na.rm = TRUE),
            avg_power = mean(Engine.HP, na.rm = TRUE))


# Odp.: najwyższą medianę sugerowanej ceny w kategorii Luxury,Performance mają samochody Volkswagen, zaś najniższą Hyundai
#dla najwyższej mediany: 
#   Make    avg_cylin  avg_power
#Volkswagen   10.7       397
#, dla najniższej mediany: 
#  Hyundai     6         311

# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?
data_9 <- data %>% 
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize")

quantile(data_9$city.mpg, probs = c(0.1,0.9, na.rm = TRUE))
#możliwe zmiana jednostek
# Odp.: 0.1 kwantyl to 14, zaś 0.9 kwantyl to 25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?
#tutaj na pewno nie chodziło o kolumne Popularity bo każde Porsche miało takie samo

data %>% 
  filter(Make == "Porsche", Engine.HP <= 300) %>% 
  group_by(Model) %>% 
  summarize(Popularnosc = n()) %>% 
  arrange(-Popularnosc)
  
# Odp.: model Porsche 944 i Cayenne występujący 4 razy


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
kombinacja <- data %>% 
  select(Engine.Fuel.Type, Driven_Wheels, Engine.HP, highway.MPG) %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  mutate(mediana_koni = median(Engine.HP)) %>% 
  arrange(-mediana_koni)

data %>% 
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)", Driven_Wheels == "all wheel drive") %>% 
  summarize(avg_mpg = mean(highway.MPG)) %>% 
  mutate(l_per_100 = 235.21 /avg_mpg) %>% 
  select(l_per_100)


# Odp.:
#a) flex-fuel (premium unleaded required/E85)    all wheel drive  608 - największa mediana
#b) 12.2187


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?
data %>% 
  mutate(city_l_100 = 235.21 / city.mpg) %>% 
  mutate(highway_l_100 = 235.21 / highway.MPG) %>% 
  select(Make, Model, highway_l_100, city_l_100) %>% 
  mutate(roznica = city_l_100 - highway_l_100) %>% 
  group_by(Make, Model) %>% 
  summarize(srednia_roznica = mean(roznica, na.rm = TRUE)) %>% 
  arrange(-srednia_roznica)

# Odp.: Ferrari     Enzom różnica 14.0 

