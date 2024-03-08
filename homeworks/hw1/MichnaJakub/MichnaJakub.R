library(dplyr)

data <- read.csv("E:/Dokumenty/Studia PW/materiały/Semestr IV/Wstęp do eksporacji/Labiska/Pursko domowskie 1/cars.csv")


# Zadanie 0 ---------------------------------------------------------------
# Jaka marka samochodu najczesciej wystepuje w zbiorze danych?

data %>%
  group_by(Make) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  head(1)

# Odp.: Chevrolet   


# Zadanie 1 ---------------------------------------------------------------
# Jaka jest mediana mocy silnika dla samochodów w zależności od rodzaju
# skrzyni biegów?

data %>%
  group_by(Transmission.Type) %>%
  summarise(mediana_mocy = median(Engine.HP, na.rm = TRUE))

# Odp.: 1 AUTOMATED_MANUAL           220
      # 2 AUTOMATIC                  253
      # 3 DIRECT_DRIVE               147
      # 4 MANUAL                     172
      # 5 UNKNOWN                    125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

zad2 <- data %>%
  filter(Make == 'Mercedes-Benz') %>%
  filter(Engine.Fuel.Type == 'diesel')

a <- zad2$city.mpg
IQR(a, na.rm = TRUE)

# Odp.: 7


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>%
  filter(Engine.Cylinders == 4) %>%
  group_by(Make) %>%
  summarise(srednia_cena = mean(MSRP, na.rm = TRUE)) %>%
  arrange(-srednia_cena) %>%
  head(1)



# Odp.:Alfa Romeo        61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
najczestszy_naped <- data %>%
  filter(Make == "BMW") %>%
  group_by(Driven_Wheels) %>%
  summarise(n = n()) %>%
  head(1)[[1]]

data %>%
  filter(Make == "BMW", Driven_Wheels == najczestszy_naped) %>%
  group_by(Model) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  head(1)[[1]]
  


# Odp.: Najwięcej jest BMW z napędem tylnim. Model: "3 Series"

# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>%
  filter(Make == "Volkswagen", Number.of.Doors == 2 | Number.of.Doors == 4) %>%
  group_by(Number.of.Doors) %>%
  summarise(mediana_mocy = median(Engine.HP, na.rm = TRUE))


# Odp.:  Number.of.Doors mediana_mocy
           # 2             170
           # 4             200


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

# 1 galon =~ 3,78l
# 1 mila =~ 1,61 km

data %>%
  group_by(Year, Make) %>%
  summarise(srednie_spalanie = mean(city.mpg, na.rm = TRUE)) %>%
  arrange(srednie_spalanie) %>%
  mutate(srednie_spalanie_L100 = 3.78 * (100/(srednie_spalanie*1.61))) %>%
  select(-srednie_spalanie) %>%
  head(1)


# Odp.:2008 Bugatti                  29.3 l/100km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
lata_style <- data %>%
  filter(Year > 2006 & Year < 2018) %>%
  group_by(Year, Vehicle.Style) %>%
  summarise(sred_popularnosc = mean(Popularity, na.rm = TRUE)) %>%
  arrange(Year,-sred_popularnosc) %>%
  rename(Year1 = Year)

lata_maxliczby <- lata_style %>%
  group_by(Year1) %>%
  summarise(najpop_liczba = max(sred_popularnosc)) %>%
  rename(Year2 = Year1)


lata_maxliczby %>%
  inner_join(lata_style, join_by(najpop_liczba == sred_popularnosc, Year2 == Year1)) %>%
  select(-najpop_liczba)

# Odp.:
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

# Najczęśćiej powtarzającym się stylem był Cargo Minivan


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?



# Odp.:
     

# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?



# Odp.:


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?



# Odp.:


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  


# Odp.:


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?



# Odp.:

