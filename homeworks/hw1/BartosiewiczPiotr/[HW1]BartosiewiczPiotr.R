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
  summarise(mediana = median(Engine.HP, na.rm=TRUE))

# Odp.: Transmission.Type mediana (in HP)
# <chr>               <dbl>
#   1 AUTOMATED_MANUAL      220
# 2 AUTOMATIC             253
# 3 DIRECT_DRIVE          147
# 4 MANUAL                172
# 5 UNKNOWN               125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>%
  filter(Make == "Mercedes-Benz" & Engine.Fuel.Type == "diesel")
# widzimy ze jest ich 30, a więc parzyście,
#więc podzielę ten dataset na dwie ramki pomocnicze
temp_1 = data %>%
  filter(Make == "Mercedes-Benz" & Engine.Fuel.Type == "diesel") %>%
  arrange(city.mpg) %>%
  select(Make, city.mpg) %>%
  head(15)

temp_2 = data %>%
  filter(Make == "Mercedes-Benz" & Engine.Fuel.Type == "diesel") %>%
  arrange(city.mpg) %>%
  select(Make, city.mpg) %>%
  tail(15)

Q1 = temp_1 %>%
  summarise(q = median(city.mpg))

Q3 = temp_2 %>%
  summarise(q = median(city.mpg))

IQR = Q3 - Q1

# Odp.: 8 


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>%
  filter(Engine.Cylinders == 4) %>%
  group_by(Make) %>%
  summarise(mean_price = mean(MSRP, na.rm = TRUE)) %>%
  arrange(-mean_price) %>%
  head(1)

# Odp.: Alfa Romeo, 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.



data %>%
  filter(Make == "BMW") %>%
  group_by(Driven_Wheels) %>%
  summarise(n = n()) %>%
  arrange(-n)

# zakładam, że chodzi najczęściej występujący model BMW o tylnim napędzie

data %>%
  filter(Make == "BMW",Driven_Wheels == "rear wheel drive" ) %>%
  group_by(Model) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  head(1)

# Odp.: 

# Driven_Wheels         n
# <chr>             <int>
#   1 rear wheel drive    189
# 2 all wheel drive     144
# 3 front wheel drive     1

# BMW: 1 Series


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

m2 = data %>%
  filter(Number.of.Doors == 2) %>%
  summarise(mediana = median(Engine.HP, na.rm = TRUE)) %>%
  select(mediana)


m4 = data %>%
  filter(Number.of.Doors == 4) %>%
  summarise(mediana = median(Engine.HP, na.rm = TRUE)) %>%
  select(mediana)

m4 - m2

# Odp.: Zmienia sie o 19 HP z 220 HP dla 2 drzwi do 239 HP dla 4 drzwi


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>%
  group_by(Make, Year) %>%
  summarise(mean_city_mpg = mean(city.mpg, na.rm = TRUE)) %>%
  mutate(mean_city_mpg = mean_city_mpg*235) %>% #przelicznik wzięty z internetu
  arrange(-mean_city_mpg) %>%
  head(1)


# Odp.:  Make   Year mean_city_mpg
#        Tesla  2016        22743.


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?

data %>%
  filter(Year<=2017 & Year >=2007 ) %>%
  group_by(Year, Vehicle.Style) %>%
  summarise(srednia = mean(Popularity)) %>%
  arrange(-srednia)

temp = seq(1, 11, 1)

for (i in 2007:2017){
  t = data %>%
    filter(Year ==i) %>%
    group_by(Year, Vehicle.Style) %>%
    summarise(srednia = mean(Popularity)) %>%
    arrange(-srednia) %>%
    head(1) %>%
    select(Vehicle.Style)
  temp[i-2006] = pull(t) 
  }

# Odp.: Srednio najbardziej popularnym modelem samochodu byl "Regular Cab Pickup" w 2011 roku. 
# Najczęściej jako srednio najpopularniesze występowały "Cargo Minivan" oraz "Cargo Van" po 3 razy.

# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>%
  filter(Market.Category == "Luxury,Performance" & Year >= 2000) %>%
  group_by(Make) %>%
  summarise(mediana = median(MSRP, na.rm=TRUE)) %>%
  arrange(-mediana)
  
temp = data %>%
  filter(Market.Category == "Luxury,Performance" & Year >= 2000) %>%
  group_by(Make) %>%
  summarise(sr_cylindry = mean(Engine.Cylinders, na.rm = TRUE), 
            sr_moc = mean(Engine.HP, na.rm = TRUE),
            sr_cena = mean(MSRP, na.rm = TRUE)) %>%
  arrange(-sr_cena)

temp[1,2:3] - temp[15,2:3]

# Odp.:
  #   1 Volkswagen      94600
  # 2 Porsche         81400
  # 3 Land Rover      67600
#   
# sr_cylindry   sr_moc
# 1    6.095238 134.8571
  


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?
t = data %>%
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size=="Midsize") %>%
  arrange(city.mpg)
Q = quantile(t$city.mpg, probs = c(0.1, 0.9))
# Odp.: 14  25 


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>%
  filter(Make== "Porsche", Engine.HP <= 300) %>%
  group_by(Model) %>%
  summarise(pop = sum(Popularity, na.rm=TRUE)) %>%
  arrange(-pop)
  
  


# Odp.: Model: 968


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?

data %>%
  group_by(Engine.Fuel.Type, Driven_Wheels) %>%
  summarise(mediana = median(Engine.HP, na.rm=TRUE)) %>%
  arrange(-mediana) %>%
  head(1)
  
data %>%
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)", 
         Driven_Wheels== "all wheel drive") %>%
  summarise(sr_spal = mean(highway.MPG, na.rm = TRUE)) %>%
  mutate(sr_spal = sr_spal*235)
  

# Odp.:
  # Engine.Fuel.Type                          Driven_Wheels   mediana
  # 1 flex-fuel (premium unleaded required/E85) all wheel drive     608

#średnie spalanie na autostradzie dla tej kombinacji w L/100:
#   1 4523.75


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>%
  group_by(Make, Model) %>%
  summarise(sr_aut = mean(highway.MPG, na.rm = TRUE), sr_mia = mean(city.mpg, na.rm = TRUE))%>%
  mutate(roznica = abs(sr_aut*235 - sr_mia*235)) %>%
  arrange(-roznica) %>%
  head(1)

# Odp.: Make  Model   sr_aut sr_mia roznica
#  1 Kia   Soul EV     92    120    6580
