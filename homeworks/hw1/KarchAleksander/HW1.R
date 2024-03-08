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
  summarise(med = median(Engine.HP, na.rm = TRUE)) %>% 
  arrange(-med)


# Odp.: Mamy kolejno:
# 1 AUTOMATIC           253
# 2 AUTOMATED_MANUAL    220
# 3 MANUAL              172
# 4 DIRECT_DRIVE        147
# 5 UNKNOWN             125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>% 
  filter(Make == "Mercedes-Benz", Engine.Fuel.Type =="diesel") %>% 
  summarise(iqr = IQR(city.mpg))

# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders == 4) %>% 
  group_by(Make) %>% 
  summarise(reg_price = mean(MSRP, na.rm = TRUE)) %>% 
  arrange(-reg_price)

# Odp.: Alfa Romeo, 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>% 
  filter(Make == "BMW") %>% 
  group_by(Driven_Wheels) %>%
  summarise(n = n()) %>%
  arrange(-n)

data %>% 
  filter(Make == "BMW", Driven_Wheels =="rear wheel drive") %>% 
  group_by(Model) %>%
  summarise(n = n()) %>%
  arrange(-n)

# Odp.: Najwięcej jest z napędęm na tył. Model: 1 series


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make == "Volkswagen") %>% 
  group_by(Number.of.Doors) %>%
  summarise(med = median(Engine.HP, na.rm = TRUE)) %>%
  arrange(-med)


# Odp.: Dla 2 drzwi = 170hp, dla 4 - 200hp


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>% 
  group_by(Make, Year) %>% 
  summarise(CityL100km = 235.214583/mean(city.mpg, na.rm = TRUE)) %>% 
  arrange(-CityL100km)


# Odp.: Ex aequo Bugatti rocznik 2009 i 2008 - 29.4 L/100km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
data1<-data %>% 
  filter(Year<=2017, Year >= 2007) %>% 
  group_by(Year, Vehicle.Style) %>%
  summarise(pplr = mean(Popularity, na.rm = TRUE)) %>%
  arrange(-pplr) 
part <- data1 %>% 
  group_by(Year) %>%
  summarise(mpl = max(pplr)) %>% 
  arrange(Year)
data1 %>% 
  filter(pplr %in% part$mpl) %>% 
  arrange(Year)
# Odp.:
# 1  2007 Cargo Minivan       
# 2  2008 Crew Cab Pickup     
# 3  2009 Extended Cab Pickup 
# 4  2010 Extended Cab Pickup 
# 5  2011 Regular Cab Pickup
# 6  2012 Cargo Van  and Passenger Van
# 7  2013 Cargo Van  and Passenger Van          
# 8  2014 Cargo Van           
# 9  2015 Cargo Minivan        
# 10  2016 Cargo Minivan       
# 11  2017 Passenger Van   
# Najczęściej wystąpiły Cargo Van, Passenger Van  i Cargo Minivan ex aequo

# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?
data %>% 
  filter(Year>=2000, Market.Category == "Luxury,Performance") %>% 
  group_by(Make) %>% 
  summarise(med = median(MSRP, na.rm = TRUE)) %>% 
  arrange(-med)

data %>% 
  filter(Year>=2000, Market.Category == "Luxury,Performance", Make %in% c("Volkswagen", "Hyundai")) %>% 
  group_by(Make) %>% 
  reframe(mhp = mean(Engine.HP, na.rm = TRUE), mc = mean(Engine.Cylinders, na.rm = TRUE))

  
  


# Odp.: Najwyższą Volkswagen, najniższą Hyundai.
# Marka        średnia moc    średnia ilość cylindrów
# Hyundai      311            6  
# Volkswagen   397            10.7

# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?
data %>% 
  filter(Vehicle.Size == "Midsize", Transmission.Type == "AUTOMATIC") %>% 
  reframe(t = quantile(city.mpg, c(0.1, 0.9)))


# Odp.: 0.1 kwantyl = 14, 0.9 kwantyl = 25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>% 
  filter(Make =="Porsche", Engine.HP <= 300) %>% 
  select(Model, Popularity) %>% 
  arrange(-Popularity)


# Odp.: Wszystkie modele są tak samo popularne, więc teoretycznie żaden nie jest drugi najpopularniejszy


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  summarise(med = median(Engine.Cylinders, na.rm = TRUE)) %>% 
  arrange(-med)

data %>%
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)", Driven_Wheels == "all wheel drive") %>% 
  summarise(MotorwayL100km = 235.214583/mean(highway.MPG, na.rm = TRUE))
  

# Odp.:"flex-fuel (premium unleaded required/E85)"    all wheel drive 
# srednie spalanie wynosi 12.21894 L/100km


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>% 
  group_by(Make, Model) %>% 
  summarise(diff = abs(235.214583/mean(highway.MPG, na.rm = TRUE)-235.214583/mean(city.mpg, na.rm = TRUE))) %>% 
  arrange(-diff)

# Odp.: Ferrari Enzo, 14.0 L/100km

