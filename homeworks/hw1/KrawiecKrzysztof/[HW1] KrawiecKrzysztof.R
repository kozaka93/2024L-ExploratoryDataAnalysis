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

data%>%
  group_by(Transmission.Type)%>%
  summarise(Mediana = median(Engine.HP, na.rm = TRUE))%>%
  arrange(-Mediana)

# Odp.:
# A tibble: 5 × 2
#Transmission.Type Mediana
#<chr>               <dbl>
#1 AUTOMATIC             253
#2 AUTOMATED_MANUAL      220
#3 MANUAL                172
#4 DIRECT_DRIVE          147
#5 UNKNOWN               125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data%>%
  select(Make, Engine.Fuel.Type, city.mpg)%>%
  filter(Engine.Fuel.Type == "diesel", Make == "Mercedes-Benz")%>%
  summarise(IQR = IQR(city.mpg, na.rm = TRUE))
  

# Odp.:
#IQR
#1 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data%>%
  filter(Engine.Cylinders == 4)%>%
  group_by(Make)%>%
  summarise(msrp = mean(MSRP))%>%
  arrange(-msrp)

# Odp.: Alfa Romeo - 61600

# A tibble: 35 × 2
#Make            msrp
#<chr>          <dbl>
#1 Alfa Romeo    61600 
#2 Lotus         59359.
#3 Land Rover    46571.
#4 Cadillac      45864.
#5 BMW           41582.
#6 Audi          41269.
#7 Mercedes-Benz 41253.
#8 Lincoln       40343.
#9 Lexus         38861.
#10 Infiniti      34060.


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data%>%
  filter(Make == "BMW")%>%
  group_by(Driven_Wheels)%>%
  summarise(Count = n())%>%
  arrange(-Count)

data%>%
  filter(Make == "BMW", Driven_Wheels == "rear wheel drive")%>%
  group_by(Model) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(3)
  

# Odp.: Najwięcej jest aut z napędem na tył -  189:
# A tibble: 3 × 2
#Driven_Wheels     Count
#<chr>             <int>
#1 rear wheel drive    189
#2 all wheel drive     144
#3 front wheel drive     1

#Najczęściej występującym modelem samochodu jest BMW 1 Series

# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make == "Volkswagen") %>% 
  filter(Number.of.Doors == 2 | Number.of.Doors == 4) %>% 
  group_by(Number.of.Doors) %>% 
  summarise(Mediana = median(Engine.HP, na.rm = TRUE))
  

# Odp.: Wraz ze zwiększającą się liczbą drzwi od 2 do 4 (nie licząc 3) mediana rośnie


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

#Załóżmy, że 1 mila, to 1,61km, a 1 galon, to 3,785 litra

data %>% 
  group_by(Year, Make)%>%
  summarise(mean = mean(city.mpg, na.rm =TRUE)) %>% 
  mutate(srednia.na.100km = (3.785/(mean*1.61))*100) %>% 
  arrange(-srednia.na.100km)
  
# Odp.: Najwięcej pali Bugatti z 2008 i 2009 roku - 29,4l/100km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?

#Funkcja pomocnicza na modę:

Moda <- function(x) {
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
}


data %>% 
  filter(Year >= 2007 & Year <= 2017) %>% 
  group_by(Year) %>% 
  select(Year, Vehicle.Style) %>% 
  summarise(moda = Moda(Vehicle.Style))

#Stąd mi wyszło, że najbardziej popularnym (najczęściej występującym) stylem w 
#latach 2007-2009, 2014, 2017 był SUV, natomiast w pozostałych latach - Sedan.
#Teraz policzę tak, jakby chodziło o miarę popularności

data %>%  
  filter(Year >= 2007 & Year <= 2017) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(mean = mean(Popularity, na.rm = TRUE)) %>% 
  group_by(Year) %>% 
  filter(mean == max(mean)) #ta linia kodu pozwala nam wybrać najwyższy wynik w danym roku
  

# Odp.:
# A tibble: 11 × 2
#Year moda   
#<int> <chr>  
#  1  2007 4dr SUV
#2  2008 4dr SUV
#3  2009 4dr SUV
#4  2010 Sedan  
#5  2011 Sedan  
#6  2012 Sedan  
#7  2013 Sedan  
#8  2014 4dr SUV
#9  2015 Sedan  
#10  2016 Sedan  
#11  2017 4dr SUV      <------------------ to dla mody

# A tibble: 13 × 3
# Groups:   Year [11]
#Year Vehicle.Style        mean
#<int> <chr>               <dbl>
#  1  2007 Cargo Minivan       2964.
#2  2008 Crew Cab Pickup     2461.
#3  2009 Extended Cab Pickup 3583.
#4  2010 Extended Cab Pickup 3093 
#5  2011 Regular Cab Pickup  5657 
#6  2012 Cargo Van           5657 
#7  2012 Passenger Van       5657 
#8  2013 Cargo Van           5657 
#9  2013 Passenger Van       5657 
#10  2014 Cargo Van           2858.
#11  2015 Cargo Minivan       4337 
#12  2016 Cargo Minivan       4051.
#13  2017 Passenger Van       5657  <----------------- to dla miary popularności


data %>%   
  filter(Year>=2007& Year<=2017) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarize(mean = mean(Popularity)) %>% 
  group_by(Year) %>% 
  filter(mean == max(mean)) %>% 
  group_by(Vehicle.Style) %>% 
  summarize(n = n()) %>% 
  arrange(-n)

# A tibble: 6 × 2
#Vehicle.Style           n
#<chr>               <int>
#  1 Cargo Minivan           3
#2 Cargo Van               3
#3 Passenger Van           3
#4 Extended Cab Pickup     2
#5 Crew Cab Pickup         1
#6 Regular Cab Pickup      1

#Wychodzi na to, że najbardziej popularnym stylem przez te 10 lat był Cargo Minivan,
#Cargo Van i Passenger Van.



# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
  filter(Market.Category == "Luxury,Performance", Year >= 2000) %>% 
  group_by(Make) %>% 
  summarise(median = median(MSRP, na.rm = TRUE)) %>% 
  arrange(-median)

data %>% 
  filter(Market.Category == "Luxury,Performance", Year >= 2000) %>% 
  group_by(Make) %>% 
  summarise(median = median(MSRP, na.rm = TRUE)) %>% 
  arrange(median)

data %>%
  filter(Market.Category == "Luxury,Performance", Year >= 2000) %>% 
  group_by(Make) %>% 
  summarise(cylinder_mean = mean(Engine.Cylinders, na.rm = TRUE), hp_mean = mean(Engine.HP, na.rm = TRUE)) %>% 
  filter(Make == "Volkswagen" | Make == "Hyundai")
  



# Odp.: Samochody marki Volkswagen mają najwyższą medianę, a Hyundai najniższą.
# Hyundai ma średnią liczbę cylindrów na poziomi 6, a Volkswagen na poziomi aż 10,7.
# Z mocą nie ma tak drastycznej różnicy - Hyundai: 311, Volkswagen: 397.

# A tibble: 15 × 2
# Make          median
# <chr>          <dbl>
#   1 Volkswagen     94600
# 2 Porsche        81400
# 3 Land Rover     67600
# 4 Audi           60100
# 5 Mercedes-Benz  55975
# 6 Cadillac       50050
# 7 BMW            48950
# 8 Lexus          48600
# 9 Infiniti       45700
# 10 Acura          45285
# 11 Genesis        42650
# 12 Saab           42000
# 13 Lincoln        39970
# 14 Volvo          39650
# 15 Hyundai        39625

# A tibble: 2 × 3
# Make       cylinder_mean hp_mean
# <chr>              <dbl>   <dbl>
#   1 Hyundai              6       311
# 2 Volkswagen          10.7     397


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>% 
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>% 
  summarise(quantile = quantile(city.mpg, na.rm = TRUE, probs= c(0.1, 0.9)))
  

# Odp.: 0,1 kwantyl wynosi 14, a 0,9 kwantyl 25 (ofc w milach na galon)

# quantile
# 1       14
# 2       25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>% 
  filter(Make == "Porsche", Engine.HP <= 300) %>% 
  group_by(Model) %>% 
  summarise(popularity = mean(Popularity)) %>% 
  arrange(-popularity)


# Odp.: Drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych, to Porsche 944, ale jest 8 
#takich samych wyników:

# A tibble: 8 × 2
# Model      popularity
# <chr>           <dbl>
#   1 718 Cayman       1715
# 2 944              1715
# 3 968              1715
# 4 Boxster          1715
# 5 Cayenne          1715
# 6 Cayman           1715
# 7 Cayman S         1715
# 8 Macan            1715


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  summarise(mediana = median(Engine.HP, na.rm = TRUE)) %>% 
  arrange(-mediana) %>% 
  head(1)

data %>%
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)", 
         Driven_Wheels == "all wheel drive") %>% 
  summarise(mean = mean(highway.MPG, na.rm = TRUE)) %>% 
  mutate(mean = (3.785/(mean*1.61))*100)

# Odp.: Mediana koni mechanicznych jest największa dla all wheel drive o flex fuel (premium unleaded required/E85),
# natomiast średnie spalanie na autostradzie dla tej kombinacji w L/100 wynosi 12,21 l/100km

# # A tibble: 1 × 3
# # Groups:   Engine.Fuel.Type [1]
# Engine.Fuel.Type                          Driven_Wheels   mediana
# <chr>                                     <chr>             <dbl>
#   1 flex-fuel (premium unleaded required/E85) all wheel drive     608

# mean
# 1 12.21263



# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>%
  group_by(Make, Model) %>% 
  summarise(mean.city = mean(city.mpg, na.rm = TRUE), mean.highway = mean(highway.MPG, na.rm = TRUE)) %>% 
  mutate(mean.city.lp100km = (3.785/(mean.city*1.61))*100, mean.highway.lp100km = (3.785/(mean.highway*1.61))*100) %>% 
  mutate(diff = abs(mean.city.lp100km - mean.highway.lp100km)) %>% 
  arrange(-diff) %>% 
  select(Make, Model, diff) %>% 
  head(1)

# Odp.: Ferrari Enzo - 14 l/100km

# A tibble: 1 × 3
# Groups:   Make [1]
# Make    Model  diff
# <chr>   <chr> <dbl>
#   1 Ferrari Enzo   14.0

