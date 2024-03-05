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
  summarise(med = median(Engine.HP,  na.rm = TRUE))%>%
  arrange(-med)

# Odp.:
#Transmission.Type   med

#1 AUTOMATIC           253
#2 AUTOMATED_MANUAL    220
#3 MANUAL              172
#4 DIRECT_DRIVE        147
#5 UNKNOWN             125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?
data %>%
  filter(Make == 'Mercedes-Benz', Engine.Fuel.Type == 'diesel')  %>% 
  summarise(iqr = IQR(city.mpg,  na.rm = TRUE))
  
# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>%
  filter(Engine.Cylinders == 4)  %>% 
  group_by(Make) %>%
  summarise(mean = mean(MSRP,  na.rm = TRUE))%>%
  arrange(-mean)%>%
  head(1)


# Odp.: Alfa Romeo


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.

data %>%
  filter(Make == 'BMW') %>%
  group_by(Driven_Wheels) %>%
  summarise(n = n())%>%
  arrange(-n)

data %>%
  filter(Make == 'BMW', Driven_Wheels == 'rear wheel drive') %>%
  count(Model)%>%
  arrange(-n)%>%
  head(1)

# Odp.: Najwięcej jest aut BMW z napędem na tył, następnie na 4 koła, a najmniej z napędem na przód. 
#Najczęściej występujący model dla napędu na tył to 1 Series.


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>%
  filter(Make == 'Volkswagen', Number.of.Doors == 2 | Number.of.Doors == 4) %>%
  group_by(Number.of.Doors) %>%
  summarise(med = median(Engine.HP,  na.rm = TRUE))%>%
  arrange(-med)

# Odp.:
#Number.of.Doors   med

#1               4   200
#2               2   170


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>%
  mutate(spal = 235/city.mpg)%>%
  group_by(Year, Make) %>%
  summarise(sr_spal = mean(spal,  na.rm = TRUE))%>%
  arrange(-sr_spal)

# Odp.: 2008/2009 Bugatti, 29.4 L/100km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?

data %>% 
  filter(Year >= 2007, Year <= 2017)%>%
  select(Year,Vehicle.Style,Popularity)%>%
  group_by(Year) %>%
  arrange(-Popularity)%>%
  summarise(mostpop=first(Vehicle.Style))

data %>% 
  filter(Year >= 2007, Year <= 2017) %>%
  group_by(Vehicle.Style) %>%
  summarise(sr_pop = sum(Popularity/11)) %>% 
  arrange(-sr_pop) %>% 
  head(1)
  
# Odp.: Średnio najbardziej popularny przez te 10 lat był Sedan.
#Średnio najbardziej popularny styl w latach 2007-2017:
  #Year mostpop        
      
#1  2007 Sedan          
#2  2008 Crew Cab Pickup
#3  2009 Sedan          
#4  2010 Sedan          
#5  2011 Sedan          
#6  2012 Cargo Van      
#7  2013 Cargo Van      
#8  2014 Cargo Van      
#9  2015 Wagon          
#10  2016 Wagon          
#11  2017 Wagon 

# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
  filter(Market.Category == "Luxury,Performance", Year >= 2000) %>% 
  group_by(Make) %>%
  summarise(med_MSRP = median(MSRP,  na.rm = TRUE), mean_Engine.Cylinders = mean(Engine.Cylinders, na.rm = TRUE), mean_Engine.HP = mean(Engine.HP, na.rm = TRUE))%>%
  arrange(-med_MSRP) %>% 
  slice(c(1,n()))


# Odp.:Najwyższą medianę ma Volkswagen, 

#Make          med_MSRP mean_Engine.Cylinders  mean_Engine.HP

#1 Volkswagen    94600                  10.7            397
#2 Hyundai       39625                   6              311


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>%
  filter(Transmission.Type == 'AUTOMATIC', Vehicle.Size == 'Midsize')  %>% 
  summarise(q0.1 = quantile(city.mpg,probs = 0.1), q0.9 = quantile(city.mpg,probs = 0.9))
  

# Odp.: 14; 25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>%
  filter(Make == 'Porsche', Engine.HP<=300) %>% 
  arrange(-Popularity) %>% 
  select(Model,Popularity)
  

# Odp.: wszystkie są tak samo popularne


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?

data %>% 
  group_by(Engine.Fuel.Type,Driven_Wheels) %>%
  summarise(med = median(Engine.HP,  na.rm = TRUE))%>%
  arrange(-med) %>% 
  head(1)

# Odp.: flex-fuel (premium unleaded required/E85), all wheel drive


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>% 
  mutate(spal_rozn = 235/city.mpg - 235/highway.MPG)%>%
  group_by(Make,Model) %>%
  summarise(sr_spal = mean(spal_rozn,  na.rm = TRUE))%>%
  arrange(-sr_spal)
  
# Odp.: Ferrari Enzo, rożnica wynosi 14L/100km.