library(dplyr)

data <- read.csv("/Users/nataliagajda/Downloads/data.csv")
View(data) #wyświetle sobie bo łatwiej mi się tak pracuje jak wiem co gdzie jest

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
  summarise(kwartyle = quantile(city.mpg, na.rm = TRUE))



# Odp.: 7.25 (różnica między 3 a 1, w tym przypadku 4 a 2 bo taka numeracja w R)
# kwartyle
# 1    18.00
# 2    19.00
# 3    22.00
# 4    26.25
# 5    28.00


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>%
  filter(Engine.Cylinders == '4') %>%
  group_by(Make) %>%
  summarise(Mean.Price = mean(MSRP)) %>%
  arrange(-Mean.Price) %>%
  head(1)


# Odp.: Alfa Romeo, cena to 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.

data %>%
  filter(Make == "BMW") %>%
  group_by(Driven_Wheels) %>%
  summarise(How.Many = n()) %>%
  arrange(-How.Many) %>%
  head(1) 

# z pierwszej części kodu dostajemy odpowiedź rear wheel drive, więc potem tworzę 
# funkcję i szukam w tej kategorii 

data %>%
  filter(Driven_Wheels == "rear wheel drive" & Make == "BMW") %>%
  group_by(Model) %>%
  summarise(How.Many.M = n()) %>%
  arrange(-How.Many.M) %>%
  head(1)
  


# Odp.: Z napędem na tył: rear wheel drive (189), najczęściej występujący model: 1 Series (16)


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>%
  filter(Make == "Volkswagen" & Number.of.Doors == c(2,4)) %>%
  group_by(Number.of.Doors) %>%
  summarise(Mediana = median(Engine.HP, na.rm = TRUE))


# Odp.: Number.of.Doors Mediana
#1               2     170
#2               4     200
# Mediana zwiększa się o 30 w przypadku 4 drzwiowego samochodu.


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

#1 galon = 3,78541178 l , 1 mila = 1,609344 km (google)

data %>%
  group_by(Year, Make) %>%
  filter(!is.na(city.mpg)) %>%
  summarise(City.LpKm = 1/mean(city.mpg) * 3.78541178 * 100 / 1.609344 ) %>%
  arrange(-City.LpKm) %>%
  head(1)
  
# Odp.: 2008 Bugatti, spalanie w L/100km: 29.4


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
data %>%
  filter(Year >= 2007 & Year <= 2017) %>%
  group_by(Year, Vehicle.Style) %>%
  summarise(HowPopular = sum(Popularity)) %>%
  group_by(Vehicle.Style) %>%
  summarise(HowPopularOverYears = mean(HowPopular, na.rm = TRUE)) %>%
  arrange(-HowPopularOverYears) %>%
  head(1)

data %>%
  filter(Year >= 2007 & Year <= 2017) %>%
  group_by(Year, Vehicle.Style) %>%
  summarise(HowPopular = sum(Popularity)) %>%
  arrange(-HowPopular) %>%
  group_by(Year) %>%
  slice_head(n = 1)


#(Nie do końca zrozumiałam polecenie, ale odpowiem tak jak zrozumiałam)
# Odp.: Najczęściej na przestrzeni lat jako najbardziej popularne pojawiają się 
# 4dr SUV i Sedan, najpopularniejszy w na przestrzeni lat był Sedan.

# Vehicle.Style HowPopularOverYears
# 1 Sedan                     294218.

# Year Vehicle.Style       HowPopular
# 1  2007 Extended Cab Pickup      54885
# 2  2008 Crew Cab Pickup         113216
# 3  2009 Crew Cab Pickup         107887
# 4  2010 4dr SUV                  78552
# 5  2011 4dr SUV                  88240
# 6  2012 4dr Hatchback            55696
# 7  2013 4dr Hatchback            77932
# 8  2014 4dr SUV                 144849
# 9  2015 Sedan                   986057
# 10  2016 Sedan                   998724
# 11  2017 Sedan                   776483


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>%
  filter(Market.Category == "Luxury,Performance" & Year >= 2000) %>%
  group_by(Make) %>%
  summarise(Median = median(MSRP, na.rm = TRUE)) %>%
  arrange(-Median) %>%
  slice(c(1, n()))

#Obcinam pierwszą i ostatnią wartość, ponieważ w drugiej części zadania potrzebuje też tej ostatniej 
#Z pierwszej części dostaje informację o tym jaka marka ma najwyższą i najniższą medianę
#Najniższą ma Hyundai (39625)

data %>%
  filter(Market.Category == "Luxury,Performance" & Year >= 2000) %>%
  group_by(Make) %>%
  summarise(Mean.E.C = mean(Engine.Cylinders, na.rm = TRUE), Mean.E.HP = mean(Engine.HP, na.rm = TRUE)) 


# Odp.: Najwyższa mediana sugerowanej ceny: Volkswagen (94600) 
#Marka       Średnia liczba cylindrów  Średnia moc 
#1 Hyundai        6                       311 
#2 Volkswagen   10.7                      397 
#Volkswagen ma średnio 4.7 cylindrów  oraz 86kM więcej. 


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>%
  filter(Transmission.Type == "AUTOMATIC" & Vehicle.Size == "Midsize") %>%
  summarise(kwantyl01 = quantile(city.mpg, 0.1), kwantyl09 = quantile(city.mpg, 0.9))
  
# Odp.:   kwantyl01 kwantyl09
#     1        14        25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>%
  filter(Make == "Porsche" & Engine.HP <= 300) %>%
  group_by(Model) %>%
  summarise(Popularity.By.Model = sum(Popularity)) %>%
  arrange(-Popularity.By.Model) 
  
# Po zrobieniu arrange można zauważyć, że są dwa równie popularne (2 najbardziej) modele

# Odp.: 944 i Cayenne (oba 6860)


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?

data %>%
  group_by(Engine.Fuel.Type, Driven_Wheels) %>%
  summarise(med = median(Engine.HP, na.rm = TRUE)) %>%
  arrange(-med) %>%
  head(1)

#korzystając z informacji uzyskanych w części 1

data %>%
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)" & Driven_Wheels == "all wheel drive") %>%
  summarise(mean_highway_LpKm = 1/mean(highway.MPG) * 3.78541178 * 100 / 1.609344 )
  


# Odp.: Dla danej kombinacji paliwa i napędu mediana jest największa i wynosi   
#       Engine.Fuel.Type                          Driven_Wheels     med
#      1 flex-fuel (premium unleaded required/E85) all wheel drive   608
# Średnia spalania na autostradzie dla tej kombinacji to 12.21894 L/100km.


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>%
  group_by(Make, Model) %>%
  summarise(SR = abs((1/mean(city.mpg) - 1/mean(highway.MPG)) * 3.78541178 * 100 / 1.609344)) %>%
  arrange(-SR) %>%
  head(1)


# Odp.:
#        Marka      Model      Różnica
#       Ferrari     Enzo        14 L/100km

