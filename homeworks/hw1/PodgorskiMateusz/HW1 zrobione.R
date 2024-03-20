library(dplyr)

data <- read.csv('R laby/data.csv')


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
  summarize(Mediana_Mocy_Silnika = median(Engine.HP, na.rm = TRUE))
  


# Odp.:
#   Transmission.Type Mediana_Mocy_Silnika
#   <chr>                            <dbl>
# 1 AUTOMATED_MANUAL                   220
# 2 AUTOMATIC                          253
# 3 DIRECT_DRIVE                       147
# 4 MANUAL                             172
# 5 UNKNOWN                            125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?
data %>% 
  filter(Make=="Mercedes-Benz",Engine.Fuel.Type == "diesel") %>%
  summarize(IQR = IQR(city.mpg, na.rm = TRUE))
  


# Odp.:7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?
data %>% 
  filter(Engine.Cylinders==4) %>% 
  group_by(Make) %>% 
  summarise(srednia=mean(MSRP)) %>% 
  arrange(desc(srednia))



# Odp.:Alfa Romeo     61600 


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>%
  filter(Make == "BMW") %>%
  group_by(Driven_Wheels) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Odp.:rear wheel drive    189


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?
data%>%
  filter(Make == "Volkswagen",Number.of.Doors==2 | Number.of.Doors==4) %>% 
  group_by(Number.of.Doors) %>% 
  summarise(Mediana_Mocy_Silnika = median(Engine.HP))



# Odp.:
# Number.of.Doors Mediana_Mocy_Silnika
#             <int>                <dbl>
# 1               2                  170
# 2               4                  200


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.


data %>%
  
  mutate(city_L_per_100km = 235.215 / city.mpg) %>%
  
  group_by(Year, Make) %>%
  
  summarise(average_city_L_per_100km = mean(city_L_per_100km, na.rm = TRUE)) %>%
  
  arrange(desc(average_city_L_per_100km))



# Odp.:2008 Bugatti                         29.4


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
zad7<-data %>%
  filter(Year >= 2007, Year <= 2017) %>%
  group_by(Year, Vehicle.Style) %>%
  summarise(mean_popularity = mean(Popularity, na.rm = TRUE)) %>%
  ungroup() %>% 
  arrange(Year, desc(mean_popularity)) %>% 
  group_by(Year) %>%
  slice(1)
zad7 %>% 
  group_by(Vehicle.Style) %>% 
  summarise(n=n())
  

# Odp.:
# 1 Cargo Minivan           3
# 2 Cargo Van               3


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?
mediana_ceny_marki <- data %>%
  filter(Year >= 2000, grepl("Luxury,Performance", Market.Category)) %>%
  group_by(Make) %>%
  summarise(Mediana_Ceny = median(MSRP, na.rm = TRUE)) %>%
  arrange(desc(Mediana_Ceny))

print(mediana_ceny_marki)


# Marka z najwyższą medianą ceny
marka_najwyzsza <- mediana_ceny_marki %>% top_n(1, Mediana_Ceny)

# Marka z najniższą medianą ceny
marka_najnizsza <- mediana_ceny_marki %>% top_n(-1, Mediana_Ceny) %>% slice(1)

# Obliczenie średniej liczby cylindrów i mocy silnika dla wybranych marek
analiza_marek <- data %>%
  filter(Make %in% c(marka_najwyzsza$Make, marka_najnizsza$Make)) %>%
  group_by(Make) %>%
  summarise(Srednia_Liczba_Cylindrow = mean(Engine.Cylinders, na.rm = TRUE),
            Srednia_Moc_Silnika = mean(Engine.HP, na.rm = TRUE))

# Wypisanie wyników analizy
print(analiza_marek)



# Odp.:

# Make          Mediana_Ceny
# <chr>                <dbl>
#   1 Maybach            470350 
# 2 Rolls-Royce        405170 
# 3 Bentley            289900 
# 4 Volkswagen          94600 
# 5 Porsche             77150 
# 6 Maserati            72000 
# 7 Land Rover          67600 
# 8 Mercedes-Benz       57050 
# 9 Audi                54100 
# 10 Cadillac            52482.
# 11 Lexus               50365 
# 12 BMW                 49950 
# 13 Infiniti            45750 
# 14 Volvo               43950 
# 15 Genesis             42650 
# 16 Saab                42000 
# 17 Acura               41735 
# 18 Lincoln             40518.
# 19 Hyundai             39625 

# Maybach       470350
# Hyundai        39625
# Make    Srednia_Liczba_Cylindrow Srednia_Moc_Silnika
# <chr>                      <dbl>               <dbl>
#   1 Hyundai                     4.67                202.
#   2 Maybach                    12                   590.

# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?




data %>%
  filter( Transmission.Type=="AUTOMATIC", Vehicle.Size == "Midsize") %>%

  summarise(
    Q10_city_mpg = quantile(city.mpg, 0.1),
    Q90_city_mpg = quantile(city.mpg, 0.9)
  )

# Odp.:
# Q10_city_mpg Q90_city_mpg
# 1           14           25

# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?


data %>%
  filter(Make == "Porsche", Engine.HP <= 300) %>%
  group_by(Model) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(2) 
# Odp.:944       4


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  



data %>%
  group_by(`Engine.Fuel.Type`, `Driven_Wheels`) %>%
  summarise(Mediana_HP = median(`Engine.HP`, na.rm = TRUE)) %>%
  arrange(desc(Mediana_HP)) 
  


data %>%
  filter(`Engine.Fuel.Type` == "flex-fuel (premium unleaded required/E85)", 
         `Driven_Wheels` == "all wheel drive"  )%>%
  summarise(Srednie_Spalanie_Autostrada_L_per_100km = mean(235.215/(highway.MPG)))
  


# Odp.: 1 flex-fuel (premium unleaded required/E85)    all wheel drive        608
# 1                                12.26253



# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?


data %>%
  mutate(
    city_L_per_100km = 235.215 / `city.mpg`, # Konwersja spalania w mieście na L/100km
    highway_L_per_100km = 235.215 / `highway.MPG`, # Konwersja spalania na autostradzie na L/100km
    roznica_spalania_L_per_100km = abs(highway_L_per_100km - city_L_per_100km) 
  ) %>%
  group_by(Make, Model) %>%
  summarise(Srednia_Roznica = mean(roznica_spalania_L_per_100km, na.rm = TRUE)) %>%
  arrange(desc(Srednia_Roznica))

# Odp.:Ferrari     Enzo                     14.0 

