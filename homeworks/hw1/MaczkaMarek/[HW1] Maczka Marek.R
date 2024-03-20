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
  summarise(mediana = median(Engine.HP, na.rm = TRUE))

# Odp.: 
# Transmission.Type       mediana

# 1 AUTOMATED_MANUAL      220
# 2 AUTOMATIC             253
# 3 DIRECT_DRIVE          147
# 4 MANUAL                172
# 5 UNKNOWN               125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>% 
  filter(Engine.Fuel.Type == "diesel", Make == "Mercedes-Benz") %>% 
  summarise(IQR = IQR(city.mpg))

# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders == 4) %>% 
  group_by(Make) %>% 
  summarise(mean = mean(MSRP)) %>% 
  arrange(-mean) %>% 
  head()
  
# Odp.: Alfa Romeo, 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>% 
  filter(Make == "BMW") %>% 
  group_by(Driven_Wheels) %>% 
  summarise(n = n())

# Driven_Wheels         n

# 1 all wheel drive     144
# 2 front wheel drive     1
# 3 rear wheel drive    189  

data %>% 
  filter(Make == "BMW", Driven_Wheels == "rear wheel drive") %>% 
  group_by(Model) %>% 
  summarise(n_model = n()) %>% 
  arrange(-n_model) %>% 
  head()

# Model           n_model

# 1 1 Series      16
# 2 3 Series      15
# 3 4 Series      15
# 4 6 Series      12
# 5 2 Series      10
# 6 7 Series       9

# Odp.: najwięcej z napędem na tył, 1 Series


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(!is.na(Number.of.Doors), Make == "Volkswagen") %>% 
  group_by(Number.of.Doors) %>% 
  summarise(median = median(Engine.HP, na.rm = TRUE))

# Number.of.Doors   median

# 1             2    170
# 2             3    90
# 3             4    200


# Odp.: nie ma zależności monotonicznej (np. im więcej drzwi tym więcej KM), 
# największa wartość mediany mocy równa 200 jest dla samochodów 4-drzwionych, 
# najmniejsza dla 3-drzwionych (równa 90), dla 2-drzwiowych 170.


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

# zakładam, definicja galonu w danych jest amerykańska (US liquid gallon),
# defincja mili, to ta międzynarodowa
# oraz rozumiem polecenie jako znalezienie największej wartości średniego
# spalania, (które zachodzi dla pewnych samochodów) i wskazania w jakiej grupie
# (Rok, Marka) się on znajduje.

data %>% 
  group_by(Year, Make) %>% 
  summarise(lpkm = max((100 * 3.785411784) / (city.mpg * 1.609344))) %>% 
  arrange(-lpkm) %>% 
  head()

# Year Make         lpkm

# 1  2003 Ferrari      33.6
# 2  2001 Ferrari      29.4
# 3  2008 Bugatti      29.4
# 4  2008 Lamborghini  29.4
# 5  2009 Bugatti      29.4
# 6  2009 Lamborghini  29.4

# druga interpretacja, to największa wartość średniej średnich spalań w każdej
# z grup (Rok, Marka)

data %>% 
  group_by(Year, Make) %>% 
  summarise(lpkm = mean((100 * 3.785411784) / (city.mpg * 1.609344))) %>% 
  arrange(-lpkm) %>% 
  head()

# Year Make         lpkm

# 1  2008 Bugatti      29.4
# 2  2009 Bugatti      29.4
# 3  2001 Ferrari      27.8
# 4  2009 Lamborghini  27.8
# 5  2008 Lamborghini  27.4
# 6  2001 Lamborghini  26.1

# Odp.: (w zależności od intrepretacji opisanej wyżej)
# największe średnie spalanie miały samochody marki
# Ferrari z 2003, wynoszące 33.6 L/100km albo
# Bugatti z 2008 i 2009 roku, wynoszące 29.4 L/100km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
# Polecenie rozumiem jako wskazanie pary (year, Vehicle.Style) o największej 
# średniej popularności

new_data <- data %>% 
  filter(2007 <= Year, Year <= 2017) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(mean_pop = round(mean(Popularity)))
  
new_data %>% 
  arrange(-mean_pop) %>% 
  head(10)

#     Year Vehicle.Style          mean_pop

# 1   2011 Regular Cab Pickup     5657 
# 2   2012 Cargo Van              5657 
# 3   2012 Passenger Van          5657 
# 4   2013 Cargo Van              5657 
# 5   2013 Passenger Van          5657 
# 6   2017 Passenger Van          5657 
# 7   2015 Cargo Minivan          4337 
# 8   2017 Cargo Minivan          4337 
# 9   2011 Extended Cab Pickup    4104
# 10  2016 Cargo Minivan          4051

# Tworzę pomocniczą ramkę zawierającą wartości maksymalnych średnich popularności
# w podziale na lata (z wartości osiąganych w grupach (Year, Vehicle.Style)).

max_pop_by_year <- new_data %>%
  group_by(Year) %>% 
  summarise(max_pop = max(mean_pop)) %>% 
  arrange(Year)

#     Year  max_pop
# 
# 1  2007    2964
# 2  2008    2461
# 3  2009    3583
# 4  2010    3093
# 5  2011    5657
# 6  2012    5657
# 7  2013    5657
# 8  2014    2858
# 9  2015    4337
# 10  2016    4051
# 11  2017    5657

new_data %>% 
  left_join(max_pop_by_year, by="Year") %>% 
  filter(mean_pop == max_pop) %>% 
  group_by(Vehicle.Style) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

# Vehicle.Style             n

# 1 Cargo Minivan           3
# 2 Cargo Van               3
# 3 Passenger Van           3
# 4 Extended Cab Pickup     2
# 5 Crew Cab Pickup         1
# 6 Regular Cab Pickup      1


# Odp.: Największą średnią popularnością cieszyły się:
# Passenger Van w latach 2012, 2013, 2017 , Regular Cab Pickup w 2011 i
# Cargo Van w 2012.
# Najczęściej występującymi średnio najbardziej popularnymi były:
# Cargo Minivan, Cargo Van i Passenger Van będące najbardziej popularne
# każdy przez 3 lata (niekoniecznie z rzędu)

# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

new_data <- data %>% 
  filter(Year >= 2000, Market.Category == "Luxury,Performance") %>% 
  group_by(Make) %>% 
  summarise(median = median(MSRP))

new_data %>% 
  arrange(-median) %>% 
  slice(1L, dim(new_data)[1])

# Make         median

# 1 Volkswagen  94600
# 2 Hyundai     39625

# Marki z najwyższą i najniższą medianą sugerowanej ceny, to odpowiednio 
# Volksvagen i Hyundai

data %>% 
  # pytanie jest "w tej kategorii", więc nie trzeba sprawdzac roku
  filter(Market.Category == "Luxury,Performance", Make %in% c("Volkswagen", "Hyundai")) %>% 
  group_by(Make) %>% 
  summarise(mean_cyl = mean(Engine.Cylinders), mean_HP = mean(Engine.HP))

# Make            mean_cyl  mean_HP

# 1 Hyundai         6       311
# 2 Volkswagen     10.7     397

# Odp.: Najwyższą medianę sugerowanej ceny mają samochody Volksvagen.
# Średnia liczba cylindrów w tej kategorii, dla samochodów Volkswagena to 10.7,
# zaś Hyundaia to 6. Jest to różnica o 4.7 cylindra, około 
# (w grubym przybliżeniu) 2 razy mniej. Natomiast wartości średniej mocy
# (w koniach mechanicznych), to odpowiednio 397 i 311, czyli różnica o 86 koni.


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>% 
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>% 
  reframe(kwantyle = quantile(city.mpg, probs = c(0.1, 0.9)))

#   kwantyle
# 1       14
# 2       25

# Odp.:odpowiednio 14 i 25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>% 
  filter(Make == "Porsche", Engine.HP <= 300) %>% 
  select(Model, Popularity)
  
# Model Popularity
# 1  718 Cayman       1715
# 2         944       1715
# 3         944       1715
# 4         944       1715
# 5         944       1715
# 6         968       1715
# 7         968       1715
# 8         968       1715
# 9         968       1715
# 10        968       1715
# 11        968       1715
# 12    Boxster       1715
# 13    Boxster       1715
# 14    Boxster       1715
# 15    Cayenne       1715
# 16    Cayenne       1715
# 17    Cayenne       1715
# 18    Cayenne       1715
# 19   Cayman S       1715
# 20     Cayman       1715
# 21     Cayman       1715
# 22     Cayman       1715
# 23      Macan       1715

# Możemy zauważyć, że samochód marki Porsche o liczbie KM nie większej niż 300
# ma tę samą popularność. Zatem w podziale na modele, jeśli bralibyśmy średnią popularności,
# ta równość pozostanie również zachowana.

# Jeśli zaś miara popularności jest addytywna i będziemy liczyć sumę, a nie średnią
# to wygra oczywiście model występujący najwięcej razy. Formalnie:

data %>% 
  filter(Make == "Porsche", Engine.HP <= 300) %>% 
  group_by(Model) %>% 
  summarise(sum = sum(Popularity)) %>% 
  arrange(-sum)

# Model        sum

# 1 968        10290
# 2 944         6860
# 3 Cayenne     6860
# 4 Boxster     5145
# 5 Cayman      5145
# 6 718 Cayman  1715
# 7 Cayman S    1715
# 8 Macan       1715


# Odp.: Zgodnie z pierwszą interpretacją:
# Nie ma takiego modelu. Każdy jest tak samo popularny
# Drugi najpopularniejszy jest model 944 i Cayenne.

# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  summarise(KM = median(Engine.HP)) %>% 
  arrange(-KM) %>% 
  head()

# Engine.Fuel.Type                             Driven_Wheels       KM

# 1 flex-fuel (premium unleaded required/E85)    all wheel drive    608
# 2 flex-fuel (premium unleaded required/E85)    four wheel drive   510
# 3 premium unleaded (recommended)               four wheel drive   420
# 4 flex-fuel (premium unleaded recommended/E85) all wheel drive    403
# 5 premium unleaded (required)                  rear wheel drive   386
# 6 premium unleaded (required)                  all wheel drive    333


data %>% 
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)",
         Driven_Wheels == "all wheel drive") %>% 
  summarise(mean = mean(100 * 3.785411784/(1.609344 * highway.MPG)))

#       mean
# 1 12.26251

# Odp.: mediana koni mechanicznych jest największa dla kombinacji typu 
# flex-fuel (premium unleaded required/E85) i napędu all wheel drive 
# (na wszystkie koła) (i wynosi 608). Średnia (średnich) wynosi 12.26251


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>% 
  group_by(Make, Model) %>% 
  summarise(różnica = abs((100 * 3.785411784/1.609344)*(1/highway.MPG - 1/city.mpg))) %>% 
  arrange(-różnica) %>% 
  head()

# Make    Model       różnica

# 1 Ferrari Enzo           14.0
# 2 Bugatti Veyron 16.4    12.6
# 3 Bugatti Veyron 16.4    12.6
# 4 Bugatti Veyron 16.4    12.6
# 5 Ferrari 575M           11.4
# 6 Ferrari 575M           11.4


# wyskakuje warning chociaż tabele mają te same wartości, co przy użyciu
# zalecanej w warningu funkcji reframe
# 
# new_data <- data %>%
#   group_by(Make, Model) %>%
#   summarise(różnica = abs((100 * 3.785411784/1.609344)*(1/highway.MPG - 1/city.mpg))) %>%
#   arrange(-różnica)
# 
# new_data_1 <- data %>%
#   group_by(Make, Model) %>%
#   reframe(różnica = abs((100 * 3.785411784/1.609344)*(1/highway.MPG - 1/city.mpg))) %>%
#   arrange(-różnica)
# 
# all(new_data == new_data_1)


# Odp.: Największa średnia róznica wynosi 14 L/100km dla samochodu marki 
# Ferrari model Enzo

# Miłego dnia dla sprawdzającego/sprawdzającej :)