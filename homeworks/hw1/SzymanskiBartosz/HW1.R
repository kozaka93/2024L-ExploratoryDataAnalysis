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
  summarise(med = median(Engine.HP, na.rm=TRUE))

# Odp.:
#Dla typu AUTOMATED_MANUAL mediana wynosi 220
#Dla typu AUTOMATED_MANUAL mediana wynosi 253
#Dla typu AUTOMATIC mediana wynosi 147
#Dla typu DIRECT_DRIVE mediana wynosi 172
#Dla typu nieznanego (UNKNOWN) mediana wynosi 125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?
data %>% 
  filter(Make == "Mercedes-Benz" & Engine.Fuel.Type == "diesel") %>% 
  summarise(IQR = quantile(city.mpg, 0.75) - quantile(city.mpg, 0.25))

# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?
  data %>% 
    filter(Engine.Cylinders == 4) %>% 
    group_by(Make) %>% 
    summarise(srednia_cena = mean(MSRP, na.rm=TRUE)) %>% 
    arrange(-srednia_cena) %>% 
    head(1)
  

# Odp.: Alfa Romeo - 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  data %>% 
    filter(Make == "BMW") %>% 
    group_by(Driven_Wheels) %>% 
    summarise(count = n()) %>% 
    arrange(-count)
  data %>% 
    filter(Make == "BMW", Driven_Wheels == "rear wheel drive") %>% 
    group_by(Model) %>% 
    summarise(count_model = n()) %>% 
    arrange(-count_model)

# Odp.: Najwiecej BMW jest z napędem na tył(rear wheel drive). Najpopularniejszy model z takim napedem to 1 Series.


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?
  data %>% 
    filter(Make == "Volkswagen", Number.of.Doors == 2) %>% 
    summarise(moc_2 = median(Engine.HP)) -> dwa
  data %>% 
    filter(Make == "Volkswagen", Number.of.Doors == 4) %>% 
    summarise(moc_2 = median(Engine.HP)) -> cztery
  cztery-dwa

# Odp.: Dla aut 4-drzwiowych moc jest większa o 30 koni mechanicznych


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.
data %>% 
    group_by(Year, Make) %>% 
    summarise(srednie_spalanie = mean(city.mpg, na.rm = TRUE)) %>% 
    arrange(srednie_spalanie) %>% 
    mutate(srednie_spalanie = 235.215/srednie_spalanie) %>% 
    head(1)
  
# Odp.: Najwiecej w miescie bali Bugatti z 2008 - 29.4 L/100km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
  popular_styles <- data %>%
    filter(Year >= 2007 & Year <= 2017) %>%
    group_by(Year, Vehicle.Style) %>%
    summarise(avg_popularity = mean(Popularity, na.rm = TRUE)) %>%
    ungroup()
  View(popular_styles)
  
  most_popular_styles <- popular_styles %>%
    group_by(Year) %>%
    filter(avg_popularity == max(avg_popularity)) %>%
    ungroup()
  most_popular_styles
  
  most_popular_styles %>%
    count(Vehicle.Style) %>% 
    arrange(-n)

# Odp.: W latach 2007-2017 kolejno srednio najpopularniejsze były: Cargo Minivan, Crew Cab Pickup,
# Extended Cab Pickup, Extended Cab Pickup, Regular Cab Pickup ,  EX AEQUO(Cargo Van, Passenger Van),
# EX AEQUO (Cargo Van, Passenger Van), Cargo Van, Cargo Minivan, Cargo Minivan, Passenger Van.
# najczęściej występujący styl przez te 10 lat, to Cargo minivan, Passenger Van i Cargo Van


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?
data %>% 
    filter(Year >= 2000) %>% 
    filter(Market.Category == "Luxury,Performance") %>% 
    group_by(Make) %>% 
    summarise(med = median(MSRP)) %>% 
    arrange(-med) -> x
  x
  
  filter(x, row_number() == 1)
  filter(x, row_number() == n())
data %>% 
  filter(Make == "Volkswagen" | Make == "Hyundai", Market.Category == "Luxury,Performance") %>% 
  group_by(Make) %>% 
  summarise(sr_cyl = mean(Engine.Cylinders), sr_moc = mean(Engine.HP)) %>% 
  mutate(roz_cyl = sr_cyl-lag(sr_cyl), roz_sr = sr_moc-lag(sr_moc))
    


# Odp.: samochody mają najwyższą medianę sugerowanej ceny  mają kolejno Volkswagen, Porsche, Land Rover. Najniższą ma za to Hyundai.
# Volkswagen ma średnią moc silnika wieksza o 86hp i dodatkowo ma średnio wiecj o cylindra 4.67 cylindra


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>% 
  filter(Vehicle.Size == "Midsize" & Transmission.Type == "AUTOMATIC") %>% 
  summarise(Q01 = quantile(city.mpg, 0.1), Q02 = quantile(city.mpg, 0.9))

# Odp.: Kwantyl 0.1 wynosi 14, a kwanty 0.9 wynosi 25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?
data %>% 
    filter(Make == "Porsche" & Engine.HP <= 300) %>% 
    arrange(-Popularity) %>% 
    filter(row_number() == 2) %>% 
    select(Model)
  
# Odp.: 944

# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
data %>% 
    group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
    summarise(med_HP = median(Engine.HP)) %>% 
    arrange(-med_HP) %>% 
    head(1)
data %>% 
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)" & Driven_Wheels == "all wheel drive") %>% 
  transmute(srednie_spalanie = 235.215/highway.MPG) %>% 
  summarise(s = mean(srednie_spalanie))


# Odp.: Ta kombinacja to flex-fuel (premium unleaded required/E85) i all wheel drive.
# Średnie spalanie dla tej komninacji wynosi w przyblizeniu 12.26253 L/100

# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?
data %>% 
  group_by(Make, Model) %>% 
  summarise(sr_miasto = 235.215/mean(city.mpg), sr_autos = 235.215/mean(highway.MPG)) %>% 
  mutate(roznica = abs(sr_miasto-sr_autos)) %>% 
  arrange(-roznica)

# Odp.: Ferrari Enzo - 14 L/100

