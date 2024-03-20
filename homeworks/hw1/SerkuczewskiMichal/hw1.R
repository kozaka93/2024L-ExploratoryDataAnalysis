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
  group_by(Transmission.Type)%>%
  summarise(Mediana.Mocy = median(Engine.HP,na.rm=TRUE))%>%
  arrange(desc(Mediana.Mocy))
  


# Odp.: AUTOMATIC


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>%
  filter(Make == "Mercedes-Benz", Engine.Fuel.Type == "diesel") %>%
  summarise(Rozstep = IQR(city.mpg))


# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>%
  filter(Engine.Cylinders == 4) %>%
  group_by(Make) %>%
  summarise(avg.MSRP = mean(MSRP)) %>%
  arrange(desc(avg.MSRP))

# Odp.: Alfa Romeo 61600.


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.

data %>%
  filter(Make=="BMW",Driven_Wheels!="all wheel drive")%>%
  group_by(Driven_Wheels) %>%
  summarise(Ilosc = n())%>%
  arrange(desc(Ilosc))
data%>%
  filter(Make=="BMW",Driven_Wheels!="rear wheel drive")%>%
  group_by(Model)%>%
  summarise(Ilosc = n())%>%
  arrange(desc(Ilosc))

# Odp.: Najwiecej jest aut z napędem na tył, najczęściej występujący model to "3 Series"


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>%
  filter(Make == "Volkswagen",Number.of.Doors!=3)%>%
  group_by(Number.of.Doors)%>%
  summarise(Mediana.Mocy = median(Engine.HP))
  


# Odp.: Mediana mocy silnika samochodów tej marki miedzy 2, a 4 drzwiami różni się o 30 (2 - 170, 4 - 200). 


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>%
  group_by(Make,Year)%>%
  summarise(Spalanie.avgInCity = mean(city.mpg))%>%
  arrange(Spalanie.avgInCity)

1/(8*1.60934/3.78541)*100


# Odp.: Bugatti 2008/2009; 29.40188 L/100km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?

zad7 <- data %>%
  filter(Year %in% seq(2007,2017))%>%
  group_by(Year,Vehicle.Style)%>%
  summarise(Popularnosc.avg = mean(Popularity))%>%
  group_by(Year)%>%
  filter(Popularnosc.avg == max(Popularnosc.avg))
zad7
zad7%>%
  group_by(Vehicle.Style)%>%
  summarise(Ilosc = n())

# Odp.:  Cargo Minivan (2007 i 2015,2016), Crew Cab Pickup (2008), Extended Cab Pickup (2009/10),  Cargo Van (2012/13/14), Passenger Van (2012/13/17)
# 2 pytanie. CargoMinivan, Cargo Van, Passenger Van


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

zad8 <- data%>%
  filter(Market.Category == "Luxury,Performance",Year >=2000)%>%
  group_by(Make)%>%
  mutate(Mediana.ceny = median(MSRP))%>%
  select(Make,Mediana.ceny)%>%
  arrange(desc(Mediana.ceny))
zad8%>%distinct()
data%>%
   filter(Make=="Volkswagen"|Make=="Hyundai",Market.Category=="Luxury,Performance")%>%
   group_by(Make)%>%
   summarise(Cylinder.avg =mean(Engine.Cylinders) ,HP.avg = mean(Engine.HP))
  

data%>%
  filter(Make==zad8%>%first())%>%
  group_by(Cylinder.avg =mean(Engine.Cylinders) ,HP.avg = mean(Engine.HP))%>%
  select(Make,Cylinder.avg,HP.avg)


# Odp.: Najwyższą medianę sugerowanej ceny ma Volkswagen: 94600
#Hyundai ma srednia cylindrow 6,  mocy 311; Volkswagen cylindrow 10.7, mocy 397


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

zad9 <- data%>%
  filter(Transmission.Type=="AUTOMATIC",Vehicle.Size=="Midsize")
quan01 <- quantile(zad9$city.mpg,0.1,na.rm = TRUE)
quan09 <- quantile(zad9$city.mpg,0.9,na.rm = TRUE)


# Odp.:  0.1 wynosi 14, 0.9. wynosi 25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?
data%>%
  filter(Make=="Porsche",Engine.HP<=300)%>%
  arrange(desc(Popularity))%>%select(Model)%>%distinct()


# Odp.:Model 718 Cayman, 944, 968, Boxster, Cayenne, Cayman S, Cayman, Macan bo wszystkie mają ten sam stopień popularności


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?

kombinacja <- data%>%
  group_by(Engine.Fuel.Type,Driven_Wheels)%>%
  summarise(Mediana.HP = median(Engine.HP))%>%
  arrange(desc(Mediana.HP))%>%
  select(Engine.Fuel.Type,Driven_Wheels)%>%
  first()

1/(mean(
  (data%>%
  filter(Engine.Fuel.Type==kombinacja$Engine.Fuel.Type,Driven_Wheels==kombinacja$Driven_Wheels))$highway.MPG
  )*1.60934/3.78541)*100
# Odp.:flex-fuel (premium unleaded required/E85) all wheel drive = 608; srednie spalanie. 12.22 L/100km.


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

zad12 <- data%>%
  group_by(Make,Model)%>%
  summarise(Difference.avg = mean(abs(city.mpg-highway.MPG)))%>%
  arrange(desc(Difference.avg))%>%
  first()
1/((zad12$Difference.avg)*1.60934/3.78541)*100

# Odp.: Kia Soul EV; roznica wynosi 8.400538
