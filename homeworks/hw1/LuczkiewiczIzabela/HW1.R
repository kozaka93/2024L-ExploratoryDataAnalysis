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
  summarise(median = median(Engine.HP, na.rm = TRUE))


# Odp.: 
 # AUTOMATED MANUAL     220
 # AUTOMATIC            253
 # DIRECT DRIVE         147
 # MANUAL               172
 # UNKNOWN              125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>% 
  filter(Make == "Mercedes-Benz", Engine.Fuel.Type == "diesel") %>% 
  summarise(IQR = IQR(city.mpg))

# Odp.: 7.25 mpg


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders == 4) %>% 
  group_by(Make) %>% 
  summarise(mean_price = mean(MSRP, na.rm = TRUE)) %>%
  arrange(-mean_price)

# Odp.: Alfa Romeo - 61 600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>% 
  filter(Make == "BMW") %>% 
  group_by(Driven_Wheels) %>% 
  summarise(n = n(), dom = dominant(Model)) %>% 
  arrange(-n)

# Odp.: Z napędem na tył - 1 Series


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make == "Volkswagen", Number.of.Doors %in% c(2, 4)) %>% 
  group_by(Number.of.Doors) %>% 
  summarise(median_power = median(Engine.HP))


# Odp.: Mediana dla 4-drzwiowych samochodów jest o 30 KM większa.


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>% 
  group_by(Year, Make) %>% 
  summarise(mean_mpg = mean(city.mpg, na.rm = TRUE)) %>% 
  mutate(mean_lkm = 235/mean_mpg) %>% 
  arrange(-mean_lkm)


# Odp.: Z dokładnością do 1 miejsca po przecinku najwięcej spala Bugatti
# z 2008 i 2009 roku - 29.4 L/100km.


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
dominant <- function(x){
  names(sort(table(x), decreasing = TRUE))[1]
}

data %>% 
  filter(Year %in% 2007:2017) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(pop = mean(Popularity, na.rm = TRUE)) %>% 
  group_by(Year) %>% 
  arrange(-pop, .by_group = TRUE) %>% 
  summarise(most_pop = first(Vehicle.Style)) -> df
  
  dominant(df$most_pop)
# Odp.: 
  # 2007 Cargo Minivan      
  # 2008 Crew Cab Pickup    
  # 2009 Extended Cab Pickup
  # 2010 Extended Cab Pickup
  # 2011 Regular Cab Pickup 
  # 2012 Cargo Van          
  # 2013 Cargo Van          
  # 2014 Cargo Van          
  # 2015 Cargo Minivan      
  # 2016 Cargo Minivan      
  # 2017 Passenger Van

  #Najczęściej wystąpił Cargo Minivan

# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

  data %>% 
    filter(Year >= 2000, Market.Category == "Luxury,Performance") %>% 
    group_by(Make) %>% 
    summarise(price = median(MSRP, na.rm = TRUE),
              cylinders = mean(Engine.Cylinders, na.rm = TRUE),
              power = mean(Engine.HP, na.rm = TRUE)) %>% 
    arrange(-price) -> df

  c("cyl_max" = df$cylinders[1],
    "cyl_min" = df$cylinders[length(df$cylinders)],
    "power_max" = df$power[1],
    "power_min" = df$power[length(df$power)])
  
# Odp.: Samochody Volkswagena mają największą mediane. Najmniejszą ma Hyundai.
#  Volkswagen ma średnią liczbe cylindrów równą 10.7 a Hyundai 6.
#  Volkswagen ma średnią moc silnika równą 397 KM a Hyundai 311 KM.
  


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>% 
    filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>% 
    summarise(quant01 = quantile(city.mpg, 0.1, na.rm = TRUE),
              quant09 = quantile(city.mpg, 0.9, na.rm = TRUE))

# Odp.: 0.1 kwantyl : 14 mpg
#       0.9 kwantyl : 25 mpg


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>% 
     filter(Make == "Porsche", Engine.HP <= 300) %>% 
     select(Model, Popularity) %>% 
     arrange(-Popularity)

# Odp.: Wszystkie są tak samo popularne


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>% 
    group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
    summarise(M_HP = median(Engine.HP, na.rm = TRUE),
              mean_lkm = 235/mean(highway.MPG, na.rm = TRUE)) %>% 
    arrange(-M_HP)


# Odp.: flex-fuel (premium unleaded required/E85), all wheel drive, 12.2 L/100 km


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>% 
    group_by(Make, Model) %>% 
    mutate(city_lkm = 235/city.mpg,
           highway_lkm = 235/highway.MPG,
           diff = abs(city_lkm - highway_lkm)) %>%
    summarise(mean_diff_lkm = mean(diff)) %>% 
    arrange(-mean_diff_lkm)
    

# Odp.: Ferrari Enzo : 14 L/100km

