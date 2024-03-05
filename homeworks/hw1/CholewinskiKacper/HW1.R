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
  summarise(mediana = median(Engine.HP,na.rm = TRUE))


# Odp.:
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
  summarise(IQR(city.mpg))

# Odp.:7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders == "4") %>% 
  group_by(Make) %>% 
  summarise(cena = mean(MSRP)) %>% 
  arrange(-cena) 


# Odp.: Alfa Romeo, 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>% 
  filter(Make == "BMW") %>% 
  group_by(Driven_Wheels) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

# Odp.: Najwięcej jest aut z napędem na tył

data %>% 
  filter(Make == "BMW", Driven_Wheels == "rear wheel drive") %>% 
  group_by(Model) %>% 
  summarise(n = n()) %>% 
  arrange(-n)
  
# Odp.: 1 Series

# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make == "Volkswagen") %>% 
  group_by(Number.of.Doors) %>%
  summarise(mediana = median(Engine.HP)) %>%
  filter(Number.of.Doors == 2 | Number.of.Doors == 4)

# Odp.:Dla 2 drzwiowych to 170, dla 4 drzwiowych to 200


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>% 
  group_by(Make, Year) %>% 
  summarise(spalanie = mean(235/city.mpg)) %>% 
  arrange(-spalanie)

# Odp.: Bugatti, 2008, 29.4 L/100km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
data %>% 
  filter(Year >= 2007, Year <= 2017) %>%
  group_by(Year, Vehicle.Style) %>%
  summarise(popularnosc = mean(Popularity, na.rm = TRUE)) %>%
  group_by(Year) %>% 
  filter(popularnosc == max(popularnosc))

# Grupując po latach otrzymujemy najpopularniejsze
# # Groups:   Year [11]
# Year Vehicle.Style       popularnosc
# 1  2007 Cargo Minivan             2964.
# 2  2008 Crew Cab Pickup           2461.
# 3  2009 Extended Cab Pickup       3583.
# 4  2010 Extended Cab Pickup       3093 
# 5  2011 Regular Cab Pickup        5657 
# 6  2012 Cargo Van                 5657 
# 7  2012 Passenger Van             5657 
# 8  2013 Cargo Van                 5657 
# 9  2013 Passenger Van             5657 
# 10  2014 Cargo Van                 2858.
# 11  2015 Cargo Minivan             4337 
# 12  2016 Cargo Minivan             4051.
# 13  2017 Passenger Van             5657 

data %>% 
  filter(Year >= 2007, Year <= 2017) %>%
  group_by(Year, Vehicle.Style) %>%
  summarise(popularnosc = mean(Popularity, na.rm = TRUE)) %>%
  group_by(Year) %>% 
  filter(popularnosc == max(popularnosc)) %>% 
  count(Vehicle.Style) %>%
  group_by(Vehicle.Style) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

# Odp.:Najczęściej wystąpiły: Cargo Minivan, Cargo Van, Passenger Van


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?
  
data %>% 
  filter(Market.Category == "Luxury,Performance", Year >= 2000) %>%
  group_by(Make) %>% 
  summarise(mediana = median(MSRP)) %>% 
  arrange(-mediana) %>% 
  top_n(1, mediana)

# Odp.: Najwyższa mediana - Volkswagen

data %>% 
  filter(Market.Category == "Luxury,Performance", Year >= 2000, Make == c("Volkswagen", "Hyundai")) %>%
  group_by(Make) %>% 
  summarise(sredniacylindrow = mean(Engine.Cylinders), sredniamoc = mean(Engine.HP))

# Odp.:
#Make       sredniacylindrow sredniamoc
#Hyundai                 6         311 
#Volkswagen             11.2       408.

# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>% 
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>% 
  summarise(q0.1 = quantile(city.mpg, 0.1), q0.9 = quantile(city.mpg, 0.9))


# Odp.: Kwantyle wynoszą odpowiednio 14 i 25 mpg


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>% 
  filter(Make == "Porsche", Engine.HP <= 300) %>% 
  arrange(-Popularity) %>% 
  View()


# Odp.: Wszystkie modele mają taką samą popularność - 1715


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  summarise(mediana = median(Engine.HP, na.rm=TRUE)) %>% 
  arrange(-mediana)

data %>% 
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)", Driven_Wheels == "all wheel drive") %>% 
  transmute(spalanie = 235.21/highway.MPG) %>% 
  summarise(srednia = mean(spalanie))

# Odp.:Największa mediana koni mechanicznych jest dla paliwa "flex-fuel (premium unleaded required/E85)"
# oraz napędu na 4 koła, a średnia spalania to 12.26227 L/100 km


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>% 
  group_by(Make, Model)%>%
  summarise(roznica = mean(235.21/city.mpg-235.21/highway.MPG)) %>% 
  arrange(-roznica)

# Odp.: Ferrari Enzo, 14.0 L/100km

