
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
  arrange(-mediana) %>% 
  select(Transmission.Type, mediana)


# Odp.: 

#   Transmission.Type     mediana
#   <chr>                 <dbl>
# 1 AUTOMATIC             253
# 2 AUTOMATED_MANUAL      220
# 3 MANUAL                172
# 4 DIRECT_DRIVE          147
# 5 UNKNOWN               125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>% 
  filter(Make == 'Mercedes-Benz', Engine.Fuel.Type=='diesel') %>% 
  summarise(iqr=IQR(city.mpg, na.rm=TRUE))

# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders==4) %>% 
  group_by(Make) %>% 
  summarise(srednia=mean(MSRP, na.rm = TRUE)) %>% 
  arrange(-srednia)

# Odp.: Alfa Romea, 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>% 
  filter(Make=='BMW') %>% 
  group_by(Driven_Wheels) %>% 
  summarise(n=n()) %>% 
  arrange(-n)

data %>% 
  filter(Make=='BMW',Driven_Wheels=='rear wheel drive' ) %>% 
  group_by(Model) %>% 
  summarise(n=n()) %>% 
  arrange(-n)



# Odp.: Najwięcej aut BMW z napędem na tył, a najczęsćiej występującym modelem 
# z napędem na tył jest BMW 1 Series.


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make=='Volkswagen') %>% 
  group_by(Number.of.Doors) %>% 
  summarise(mediana = median(Engine.HP, na.rm=TRUE)) %>% 
  filter(Number.of.Doors ==2 | Number.of.Doors ==4)
  
# Odp.: Dla dwudrzwiowych Volkswagen mediana liczby koni mechanicznych to 170,
# zaś da czterodzrwiowych mediana wynosi 200.


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

# Przyjmuję 1 mila = 1.61km, 1 galon = 3.78l

data %>% 
  group_by(Make, Year) %>% 
  summarise(srednia = mean(city.mpg, na.rm = TRUE)) %>% 
  mutate(srednia_l_km = 1/((srednia*1.61)/(100*3.78))) %>% 
  arrange(-srednia_l_km) 



# Odp.: Najwięcej spalają samochody Bugatti wyprodukowane w 2008 i 2009 roku, 
# i jest to średnio 29.3 l/100km. 


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  

# Nie jestem pewny czy powinienem wyznaczyć, który styl samochodu najczęściej 
# występował w danym roku, czy który styl miał największą srednią miarę 
# popularności. W związku z tym zrobiłem obie wersje.

# najczęstsze występowanie:

# funkcja pomocnicza wyznaczająca modę

moda <- function(v){
  u <- unique(v)
  u[which.max( tabulate( match( v, u ) ) )]
}

data %>% 
  filter(Year>=2007& Year<=2017) %>% 
  group_by(Year) %>% 
  select(Year, Vehicle.Style) %>% 
  summarise(moda = moda(Vehicle.Style))
  
# największa średnia miara popularności:

data %>%   
  filter(Year>=2007& Year<=2017) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarize(srednia = mean(Popularity)) %>% 
  group_by(Year) %>% 
  filter(srednia== max(srednia))


data %>%   
  filter(Year>=2007& Year<=2017) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarize(srednia = mean(Popularity)) %>% 
  group_by(Year) %>% 
  filter(srednia== max(srednia)) %>% 
  group_by(Vehicle.Style) %>% 
  summarize(n=n()) %>% 
  arrange(-n)


# Odp.: W latach 2007-2009, 2014, 2017 najczęsciej występującym stylem był SUV, 
# zaś w pozstałych 6 latach najczęściej występował Sedan.

# Jeżeli zaś liczyć miarę popularności, to wychodzi następująca ramka danych:

#    Year Vehicle.Style         srednia
#    <int><chr>                 <dbl>
# 1  2007 Cargo Minivan         2964.
# 2  2008 Crew Cab Pickup       2461.
# 3  2009 Extended Cab Pickup   3583.
# 4  2010 Extended Cab Pickup   3093 
# 5  2011 Regular Cab Pickup    5657 
# 6  2012 Cargo Van             5657 
# 7  2012 Passenger Van         5657 
# 8  2013 Cargo Van             5657 
# 9  2013 Passenger Van         5657 
# 10 2014 Cargo Van             2858.
# 11 2015 Cargo Minivan         4337 
# 12 2016 Cargo Minivan         4051.
# 13 2017 Passenger Van         5657 

#   Vehicle.Style           n
#   <chr>                   <int>
# 1 Cargo Minivan           3
# 2 Cargo Van               3
# 3 Passenger Van           3
# 4 Extended Cab Pickup     2
# 5 Crew Cab Pickup         1
# 6 Regular Cab Pickup      1

# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
  filter(Market.Category=='Luxury,Performance', Year>=2000) %>% 
  group_by(Make) %>% 
  summarise(mediana=median(MSRP, na.rm=TRUE)) %>% 
  arrange(-mediana)
  
data %>% 
  filter(Market.Category=='Luxury,Performance', Year>=2000, 
         Make%in%c('Volkswagen','Hyundai')) %>% 
  group_by(Make) %>% 
  summarise(cylindry=mean(Engine.Cylinders, na.rm = TRUE), 
            moc=mean(Engine.HP, na.rm = TRUE) ) %>% 
  arrange(-cylindry)
  


# Odp.: Volkswagen. W tej kategorii od 2000 roku Volkswageny mają średnio 10.7 
# cylindrów oraz 397 koni mechanicznych. 
# Za to Hyuandaie, które miały najniższą średnią cenę, maja średnio 6 cylindrów 
# i 311 koni mechanicznych.


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

# w milach na galon

data %>% 
  filter(Transmission.Type=='AUTOMATIC', Vehicle.Size=='Midsize') %>% 
  summarise(kwantyl_01= quantile(city.mpg, na.rm=TRUE, probs=c(0.1)), 
            kwantyl_09= quantile(city.mpg, na.rm=TRUE, probs=c(0.9))) 

# w litrach na 100km

data %>% 
  filter(Transmission.Type=='AUTOMATIC', Vehicle.Size=='Midsize') %>% 
  mutate(city=1/((city.mpg)*1.61/(100*3.78))) %>% 
  summarise(kwantyl_01= quantile(city, na.rm=TRUE, probs=c(0.1)), 
            kwantyl_09= quantile(city, na.rm=TRUE, probs=c(0.9)))

# Odp.: W milach na galon: 0.1 kwantyl = 14, 0.9 kwantyl = 25
# W litrach na 100 km: 0.1 kwantyl = 9.39, 0.9 kwantyl = 16.77 
# Istotne do zauważenia jest to, że ze względu na "zamianę miejscami" jednostek
# długości i objętości kwantyle 0.1 i 0.9 też się "zamieniają miejscami".


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

# Podobnie jak w zadaniu 7 są dwie wersje, przy czym, jak się okazuje, miara 
# popularności dla modeli porsche o nie więcej niż 300 koniach mechanicznych 
# jest identyczna.

data %>% 
  filter(Make=='Porsche', Engine.HP <=300) %>% 
  group_by(Model) %>% 
  summarise(n=n()) %>% 
  arrange(-n)

data %>%   
  filter(Make=='Porsche', Engine.HP <=300) %>% 
  group_by(Model) %>% 
  summarize(srednia = mean(Popularity))

# Odp.: Wedle liczby wystąpień:
# Porsche 944, Porsche Cayenne

# Wedle miary popularności:

#   Model         srednia
#   <chr>         <dbl>
# 1 718 Cayman    1715
# 2 944           1715
# 3 968           1715
# 4 Boxster       1715
# 5 Cayenne       1715
# 6 Cayman        1715
# 7 Cayman S      1715
# 8 Macan         1715


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>%
  summarise(mediana=median(Engine.HP, na.rm=TRUE)) %>% 
  arrange(-mediana) 


x <- data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>%
  summarise(mediana=median(Engine.HP, na.rm=TRUE)) %>% 
  arrange(-mediana)

x[[1]][1]
x[[2]][1]


data %>% 
  filter(Engine.Fuel.Type==x[[1]][1], Driven_Wheels == x[[2]][1]) %>% 
  mutate(highway=1/((highway.MPG)*1.61/(100*3.78))) %>% 
  summarise(srednia = mean(highway, na.rm = TRUE))

# Odp.: Najczęsciej występująca kominacja typu paliwa i rodzaju napędu to:
# paliwo: flex-fuel (premium unleaded required/E85), napęd: all wheel drive
# Ta kombinacja spala średnio 12.239 litra na 100 kilometrów


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>% 
  group_by(Make, Model) %>% 
  mutate(highway=1/((highway.MPG)*1.61/(100*3.78)), 
         city=1/((city.mpg)*1.61/(100*3.78))) %>%
  summarise(srednia_h = mean(highway, na.rm=TRUE), 
            srednia_c = mean(city, na.rm=TRUE)) %>% 
  mutate(roznica= abs(srednia_c-srednia_h)) %>% 
  arrange(-roznica)
  
# Odp.: Największa różnica jest dla Ferrari Enzo i wynosi 14 litrów na 100 km.

