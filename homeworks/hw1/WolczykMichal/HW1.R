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
  summarise(median=median(Engine.HP, na.rm = TRUE))

# Odp.:
# 1 AUTOMATED_MANUAL     220
# 2 AUTOMATIC            253
# 3 DIRECT_DRIVE         147
# 4 MANUAL               172
# 5 UNKNOWN              125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>% 
  filter(Make == "Mercedes-Benz"& Engine.Fuel.Type == "diesel") %>% 
  summarise(IQR=IQR(city.mpg))
  
  
  

# Odp.: 7,25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders==4) %>% 
  group_by(Make) %>% 
  summarise(śr = mean(MSRP)) %>% 
  arrange(-śr)

# Odp.: Alfa Romeo 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>% 
  filter(Make == "BMW") %>% 
  group_by(Driven_Wheels) %>% 
  summarise(n=n()) %>% 
  arrange(-n)

# Odp.:Napęd na tylne koła


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make == "Volkswagen") %>% 
  group_by(Number.of.Doors) %>% 
  summarise(mediana = median(Engine.HP))


# Odp.:dla 2drzwi mediana = 170, a dla 4: 200


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>% 
  mutate(L_na_100km = 1/city.mpg*(3.78541/1.60934)*100) %>% 
  arrange(-L_na_100km) %>% 
  select(Make,Year,L_na_100km)

# Odp.: Ferrari 2003   33.60215


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
data %>% 
  filter(Year>=2007 & Year<=2017) %>% 
  group_by(Year,Vehicle.Style) %>% 
  summarise(Styl =n() )%>%
  group_by(Year) %>% 
  summarise(Moda = Vehicle.Style[which.max(Styl)]) %>%
  group_by(Moda)%>%
  summarise(n=n())


# Odp.: 
#1  2007 4dr SUV
#2  2008 4dr SUV
#3  2009 4dr SUV
#4  2010 Sedan  
#5  2011 Sedan  
#6  2012 Sedan  
#7  2013 Sedan  
#8  2014 4dr SUV
#9  2015 Sedan  
#10  2016 Sedan  
#11  2017 4dr SUV
#Najpopularniejszy model to Sedan

# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
  filter(Year >= 2000& Market.Category %in% c("Luxury","Performance")) %>% 
  group_by(Make) %>% 
  summarise(Mediana = median(MSRP)) %>% 
  arrange(-Mediana)
  
data %>% 
  filter(Year >= 2000& Make %in% c("Acura","GMC")) %>% 
  group_by(Make) %>% 
  summarise(Śr_Cylindry = mean(Engine.Cylinders), Śr_Moc = mean(Engine.HP))
           
# Odp.:najniższa mediana Acura 31895 średnia liczba cylindrów 5,32 a średnia moc = 252, 
#najwyższa GMC 68325, średnia liczba cylindrów 6,52, średnia moc silnika 275


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?
data %>% 
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>% 
  summarise(pierwszy_kwantyl = quantile(city.mpg,0.1),dziewiąty_kwantyl = quantile(city.mpg,0.9))


# Odp.:0.1 = 14 i 0.9 = 25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>% 
  filter(Make == "Porsche", Engine.HP<= 300) %>% 
  arrange(-Popularity)

# Odp.: Model 944


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>% 
  group_by(Engine.Fuel.Type,Driven_Wheels) %>% 
  summarise(Mediana = median(Engine.HP)) %>% 
  arrange(-Mediana)

data %>% 
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)" & 
           Driven_Wheels =="all wheel drive") %>% 
  mutate(L_na_100km = 1/highway.MPG*(3.78541/1.60934)*100) %>% 
  summarise(Średnie_spalanie = mean(L_na_100km,na.rm=TRUE))
# Odp.: dla paliwa typu flex-fuel (premium unleaded required/E85) i napędu na wszystkie koła
#Średnie spalanie = 12.26254 L/100km


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?
data %>% 
  mutate(autostrada = 1/highway.MPG*(3.78541/1.60934)*100) %>% 
  mutate(miasto = 1/city.mpg*(3.78541/1.60934)*100) %>% 
  mutate(różnica = abs(autostrada-miasto)) %>% 
  group_by(Make,Model) %>% 
  summarise(śr_różnica = mean(różnica)) %>% 
  arrange(-śr_różnica)


# Odp.: Ferrari i różnica wynosi 14

