library(dplyr)
library(tidyr)

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
  summarise(mediana = median(Engine.HP, na.rm = TRUE)) -> odp1



# Odp.: Dla AUTOMATED_MANUAL to 220 KM, dla AUTOMATIC to 253 KM dla DIRECT_DRIVE to 147 KM,
# dla MANUAL to 172 KM, w kategorii UNKNOWN to 125 KM.


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>% 
  filter(Make == "Mercedes-Benz" & Engine.Fuel.Type == "diesel") %>% 
  summarise(rozstep_miedzykwartylowy = IQR(city.mpg, na.rm = TRUE)) -> odp2


# Odp.: Rozstęp międzykwartlowy wynosi 7,25. 


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders == 4) %>% 
  group_by(Make) %>% 
  summarise_at(vars(MSRP), list(srednia_cena = mean)) %>% 
  arrange(desc(srednia_cena)) %>% 
  head() -> odp3


# Odp.: Biorąc pod uwagę samochody z 4-cylindrowymi silnikami producent Alfa Romeo 
# podaje najwyższą średnią sugerowaną cenę detaliczną. Ta cena wynosi 61600.


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.

data %>% 
  filter(Make == "BMW") %>% 
  group_by(Driven_Wheels) %>% 
  summarise(ile_z_takim_napedem = length(Driven_Wheels)) %>% 
  arrange(desc(ile_z_takim_napedem)) -> odp4_1

data %>% 
  filter(Make == "BMW" & Driven_Wheels == "rear wheel drive") %>% 
  group_by(Model) %>% 
  summarise(ile_jakiego_modelu = length(Model)) %>% 
  arrange(desc(ile_jakiego_modelu)) %>% 
  head() -> odp4_2


# Odp.: Najwięcej jest aut BMW z napędem na tył, później na wszystkie koła a najmniej z
# napędem na przód. Dla BMW z napędem na tył najczęściej występujący model to 1 Series.


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make == "Volkswagen" & Number.of.Doors == 2) %>% 
  group_by(Number.of.Doors) %>% 
  summarise(mediana_mocy_silnika = median(Engine.HP)) %>% 
  View() 

data %>% 
  filter(Make == "Volkswagen" & Number.of.Doors == 4) %>% 
  group_by(Number.of.Doors) %>% 
  summarise(mediana_mocy_silnika = median(Engine.HP)) %>% 
  View() 

round(((200-170)/170)*100, digits = 2)

# Odp.: Dla samochodów marki Volkswagen o 4 drzwiach mediana mocy silnika to 200 KM 
# a dla samochodów marki Volkswagen o 2 drzwiach mediana mocy silnika to 170 KM.
# Dla samochodów marki Volkswagen o 4 drzwiach mediana mocy silnika była o 17,65% większa
# niż dla samochodów marki Volkswagen o 2 drzwiach. 


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

# srednie spalanie w mieście jest milach na galon
# 1/city.mpg będzie w galonach na milę
# 1 galon to 3,785 litra
# 1 mila to 0,01*1,609344*100km
# Czyli 1/city.mpg będzie równe 3,785/(0,01*1,609344) litra na 100 km


data %>% 
  mutate(spalanie_w_L_na100km = (3.785/(0.01*1.609344))*1/city.mpg) %>% 
  group_by(Year, Make) %>% 
  summarise(srednie_spalanie = mean(spalanie_w_L_na100km)) %>% 
  arrange(desc(srednie_spalanie)) %>% 
  head() -> odp6

  
# Odp.: Największe średnie spalanie mają samochody Bugatti z 2008 oraz
# samochody Bugatti z 2009 roku. To spalanie wynosi 29.39862 L/100km.


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?


data %>% 
  filter(Year >= 2007 & Year <= 2017) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(srednia_popularnosc = mean(Popularity)) %>% 
  slice_max(srednia_popularnosc)  %>% 
  select(c("Year", "Vehicle.Style")) -> odp7_1

odp7_1 %>% 
  group_by(Vehicle.Style) %>% 
  count(Vehicle.Style) %>% 
  rename("ile_danego_modelu" = "n") %>% 
  arrange(desc(ile_danego_modelu)) -> odp7_2


# Odp.:    
#    Year Vehicle.Style      
#   <int> <chr>              
# 1  2007 Cargo Minivan      
# 2  2008 Crew Cab Pickup    
# 3  2009 Extended Cab Pickup
# 4  2010 Extended Cab Pickup
# 5  2011 Regular Cab Pickup 
# 6  2012 Cargo Van          
# 7  2012 Passenger Van      
# 8  2013 Cargo Van          
# 9  2013 Passenger Van      
# 10  2014 Cargo Van          
# 11  2015 Cargo Minivan      
# 12  2016 Cargo Minivan      
# 13  2017 Passenger Van


# Style Cargo Minivan, Cargo Van i Passenger Van wystąpiły najczęściej jako średnio 
# najbardziej popularne.


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
  filter(Year >= 2000 & Market.Category == "Luxury,Performance") %>% 
  group_by(Make) %>% 
  summarise(mediana_z_ceny = median(MSRP)) %>% 
  arrange(desc(mediana_z_ceny)) -> df5

df5 %>% 
  filter(mediana_z_ceny == max(mediana_z_ceny) | mediana_z_ceny == min(mediana_z_ceny))

data %>% 
  filter(Year >= 2000 & Market.Category == "Luxury,Performance" & Make == "Volkswagen") %>% 
  summarise(srednia_liczba_cylindrow_V = mean(Engine.Cylinders), srednia_liczba_koni_mech_V = mean(Engine.HP)) 

data %>% 
  filter(Year >= 2000 & Market.Category == "Luxury,Performance" & Make == "Hyundai") %>% 
  summarise(srednia_liczba_cylindrow_H = mean(Engine.Cylinders), srednia_liczba_koni_mech_H = mean(Engine.HP)) 

# Odp.: Najwyższą medianę sugerowanej ceny mają samochody marki Volkswagen
# a najniższą - samochody marki Hyundai.
# W przypadku samochodów marki Volkswagen średnia liczba cylindrów to w przybliżeniu 10,67
# a średnia liczba koni mechanicznych to 397 KM.
# Natomiast w przypadku samochodów marki Hyundai średnia liczba cylindrów to 6
# a średnia liczba koni mechanicznych to 311 KM. 


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>% 
  filter(Transmission.Type == "AUTOMATIC" & Vehicle.Size == "Midsize") %>% 
  summarise(kwantyl_01 = quantile(city.mpg, 0.1), kwantyl_09 = quantile(city.mpg, 0.9))


# Odp.: Kwantyl 0.1 wynosi 14 a kwantyl 0.9 wynosi 25.


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>% 
  filter(Make == "Porsche" & Engine.HP <= 300) %>% 
  group_by(Model) %>% 
  summarise(srednia_popularnosc = mean(Popularity)) -> odp_10
 

# Odp.: Nie ma drugiego najbardziej popularnego modelu marki Porsche, który posiada
# nie więcej niż 300 KM. Wszystkie modele spełniające te warunki są tak samo popularne. 


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?

data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  summarise(med_koni_mech = median(Engine.HP)) %>% 
  ungroup() %>% 
  top_n(1, med_koni_mech)

data %>% 
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)" & Driven_Wheels == "all wheel drive") %>% 
  mutate(highway_w_Lna100km = (3.785/(0.01*1.609344))*1/highway.MPG) %>% 
  summarise(srednie_spalanie_na_autostradzie = mean(highway_w_Lna100km))
  

# Odp.: Dla kombinacji (flex-fuel (premium unleaded required/E85), all wheel drive)
# mediana koni mechanicznych jest największa.
# Średnie spalanie na autostradzie dla tej kombinacji wynosi 12.26118 L/100 km.


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>% 
  group_by(Make, Model) %>% 
  mutate(city_w_Lna100km = (3.785/(0.01*1.609344))*1/city.mpg,
         highway_w_Lna100km = (3.785/(0.01*1.609344))*1/highway.MPG) %>% 
  mutate(roznica_miasto_autostrada = abs(city_w_Lna100km - highway_w_Lna100km)) %>% 
  summarise(srednia_roznica = mean(roznica_miasto_autostrada)) %>% 
  ungroup() %>% 
  top_n(1, srednia_roznica)

# Odp.: Największą średnią różnicę spalania w mieście i na autostradzie ma Ferrari Enzo.
# Ta różnica wynosi 14 L/100km. 

