library(dplyr)

data <- read.csv("data.csv")
head(data)
View(data)
# Zadanie 0 ---------------------------------------------------------------
# Jaka marka samochodu najczesciej wystepuje w zbiorze danych?

data %>%
  group_by(Make) %>%
  summarise(n = n())%>%
  arrange(-n)

# Odp.: Chevrolet   


# Zadanie 1 ---------------------------------------------------------------
# Jaka jest mediana mocy silnika dla samochodów w zależności od rodzaju
# skrzyni biegów?
data%>%
  group_by(Transmission.Type)%>%
  summarise(median = median(Engine.HP,na.rm = TRUE))



# Odp.:
#1 AUTOMATED_MANUAL     220
#2 AUTOMATIC            253
#3 DIRECT_DRIVE         147
#4 MANUAL               172
#5 UNKNOWN              125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?
data%>%
  filter(Engine.Fuel.Type=="diesel" & Make == "Mercedes-Benz")%>%
  summarise(od_miedzykwartylowy = IQR(city.mpg,na.rm = TRUE))




# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?
data%>%
  filter(Engine.Cylinders == 4)%>%
  group_by(Make)%>%
  summarise(mean_price = mean(MSRP,na.rm = TRUE))%>%
  arrange(desc(mean_price))%>%
  head(1)
  


# Odp.: Alfa Romeo, 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
data%>%
  filter(Make == "BMW")%>%
  group_by(Driven_Wheels)%>%
  summarise(n = n())%>%
  arrange(desc(n))%>%
  head(1)

data%>%
  filter(Driven_Wheels == "rear wheel drive" & Make == "BMW")%>%
  group_by(Model)%>%
  summarise(n = n())%>%
  arrange(desc(n))%>%
  head(1)



# Odp.: rear wheel drive, 1 series 


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?


data%>%
  filter(Make == "Volkswagen" & Number.of.Doors == c(2, 4))%>%
  group_by(Number.of.Doors)%>%
  summarise(mediana=median(Engine.HP,na.rm = TRUE))

  
  





# Odp.: 200 - 170 = 30


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.
data%>%
  group_by(Make, Year)%>%
  mutate(spalanie = (1 / city.mpg) * (378/1.609))%>%
  summarise(mean = mean(spalanie,na.rm = TRUE))%>%
  arrange(desc(mean))%>%
  head(10)
  


# Odp.: Bugatti 2008, srednie spalanie 29.4 (l / 100km)


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?

z7 <- data%>%
  filter(Year >= 2007 & Year <= 2017)%>%
  group_by(Year, Vehicle.Style)%>%
  summarise(mean_pop = mean(Popularity))%>%
  group_by(Year) %>%
  filter(mean_pop == max(mean_pop))%>%
  arrange(desc(Vehicle.Style))

z7%>%
  group_by(Vehicle.Style)%>%
  summarise(ilosc = n())%>%
  arrange(desc(ilosc))

z7

# Odp.: 1. 2011 - Regular Cab Pickup, 2012,2013,2017 - Passenger Van, 2009,2010 - Extended Cab Pickup,
# 2008 - Crew Cab Pickup, 2012,2013,2014 - Cargo Van, 2007,2015,2016 - Cargo Minivan 
# 2. Cargo Minivan, Cargo Van, Passenger Van, 


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?
data%>%
  filter(Market.Category == "Luxury,Performance")%>%
  filter(Year >= 2000)%>%
  group_by(Make)%>%
  summarise(price_median = median(MSRP))%>%
  arrange(desc(price_median))
  


data%>%
  filter(Market.Category == "Luxury,Performance" & Make == c("Volkswagen","Hyundai"))%>%
  filter(Year >= 2000)%>%
  group_by(Make)%>%
  summarise(il_cylin = mean(Engine.Cylinders, na.rm = TRUE))

data%>%
  filter(Market.Category == "Luxury,Performance" & Make == c("Volkswagen","Hyundai"))%>%
  filter(Year >= 2000)%>%
  group_by(Make)%>%
  summarise(il_cylin = mean(Engine.HP, na.rm = TRUE))
  



# Odp.:   Najwyższa mediana: Volkswagen,     Najniższa mediana: Hyundai, róznica w ilosci cylindrow : 5.2, 
#         roznica mocy silnika: 97




# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?


zad9 <- data%>%
  filter(Transmission.Type=="AUTOMATIC",Vehicle.Size=="Midsize")
quan01 <- quantile(zad9$city.mpg,0.1,na.rm = TRUE)
quan09 <- quantile(zad9$city.mpg,0.9,na.rm = TRUE)
quan01
quan09


# Odp.: 0.1 wynosi 14, 0.9. wynosi 25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?



(data%>%
   filter(Make=="Porsche",Engine.HP<=300)%>%
   arrange(desc(Popularity)))[2,"Model"]




# Odp.:   Model:    944


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?

data%>%
  group_by(Engine.Fuel.Type, Driven_Wheels)%>%
  summarise(mediana = median(Engine.HP, na.rm = TRUE))%>%
  arrange(desc(mediana))%>%
  head(1)


data%>%
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)" & Driven_Wheels == "all wheel drive" )%>%
  mutate(spalanie = (1 / highway.MPG) * (378/1.609))%>%
  group_by(Driven_Wheels)%>%
  summarise(sr_spalanie = mean(spalanie,na.rm = TRUE))



# Odp.: flex-fuel (premium unleaded required/E85) , all wheel drive, srednie spalanie w l na 100 km: 12,2


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data%>%
  mutate(spalanie_miasto = (1 / city.mpg) * (378/1.609))%>%
  mutate(spalanie_autostrada = (1 / highway.MPG) * (378/1.609))%>%
  mutate(roznica_spalanie = abs(spalanie_miasto - spalanie_autostrada))%>%
  group_by(Model, Make)%>%
  summarise(srednia_roznica = mean(roznica_spalanie, na.rm = TRUE))%>%
  arrange(desc(srednia_roznica))
  



# Odp.: Enzo Ferrari, 14 

