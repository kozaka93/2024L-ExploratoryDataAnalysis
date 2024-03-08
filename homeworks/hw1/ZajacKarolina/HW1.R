install.packages('dplyr')
library(dplyr)
sciezka <- "C:\\Users\\User\\Desktop\\wizualizacja\\data.csv"
data <- read.csv(sciezka)


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
  filter(!is.na(Engine.HP)) %>%
  # bierzemy te wiersze, gdzie nie ma na w kolumnie dot silników
  group_by(Transmission.Type) %>% 
  # grupujemy po unikatach z kolumny dot skrzyni biegów
  summarise(median_enhp = median(Engine.HP)) %>%
  # wyznaczamy mediane mocy silnika dla kolejnych rodzajow skrzyni biegow
  arrange(median_enhp)




# Odp.:
# Transmission.Type median_enhp
# <chr>                   <dbl>
#   1 UNKNOWN                   125
# 2 DIRECT_DRIVE              147
# 3 MANUAL                    172
# 4 AUTOMATED_MANUAL          220
# 5 AUTOMATIC                 253


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście (city.mpg) samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

zad <- data %>% 
  filter(Make == 'Mercedes-Benz',Engine.Fuel.Type == 'diesel', !is.na('city.mpg')) %>% 
  #ustawiamy wartosci dot spalania rosnaco
  arrange(city.mpg) 

Q <- quantile(zad$city.mpg, probs = c(0.25,0.75)) 
  #bierzemy 1wszy i 3ci kwartyl
IQR = Q[2]-Q[1]
  #liczymy rozstęp



# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders == 4, !is.na(MSRP)) %>%
  #bierzemy te wiersze, gdzie wartosci w kol dot cylindrów wartosci sa rowne 4 a w kol dot ceny detalicznej nie ma na
  #grupujemy po marce
  group_by(Make) %>% 
  #usrednamy ceny detaliczne dla poszczegolnych marek
  summarise(srednia_cena = mean(MSRP)) %>% 
  arrange(desc(srednia_cena))

# Odp.: Alfa RoMeo ma najwyzsza sugerowana cene detaliczną przez producenta i wynosi ona 61600
# Make          srednia_cena
# <chr>                <dbl>
#   1 Alfa Romeo          61600 
# 2 Lotus               59359.
# 3 Land Rover          46571.
# 4 Cadillac            45864.
# 5 BMW                 41582.
# 6 Audi                41269.
# 7 Mercedes-Benz       41253.
# 8 Lincoln             40343.
# 9 Lexus               38861.
# 10 Infiniti            34060.

# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
zadanie4.1 <- data %>% 
  #bierzemy wiersze gdzie w kol dot marki wartosci to bmw
  filter(Make == "BMW") %>% 
  # grupujemy po rodzajach napedu
  group_by(Driven_Wheels) %>% 
  summarise(num = n()) %>% 
  arrange(desc(num))

# all wheel drive
# 144
# front wheel drive
# 1
# rear wheel drive
# 189
#odp,: najwiecej jest z napedem na tyl, potem na 4, a najmniej na przód

zad4.2 <- data %>% 
  #filtrujemy po marce bmw gdzie naped jest na 4 kola
  filter(Make == "BMW", Driven_Wheels == "rear wheel drive") %>%
  #grupujemy po marce
  group_by(Model) %>% 
  summarise(num = n()) %>% 
  arrange(desc(num)) 
  #sortowanko


# Odp.: najczęściej występujący model z napedem na 4 kola to bmw z 1 serii.

# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make == "Volkswagen", !is.na(Engine.HP)) %>% 
  #bierzemy wiersze z charakterami volkswagen i gdzie wartosci mocy silnika nie sa na
  group_by(Number.of.Doors) %>% 
  #grupujemy po liczbie drzwi
  summarise(mediana_mocy = median(Engine.HP)) %>% 
  #liczymy mediane mocy silnika dla samochodzych o tej samej liczbie drzwi
  #bierzemy wiersze gdzie liczba drzwi wynosi 2 lub 4
  filter(Number.of.Doors == 2 | Number.of.Doors == 4)

# Odp.:

# Number.of.Doors mediana_mocy
# <int>        <dbl>
#   1               2          170
# 2               4          200 
  
  


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>% 
  filter(!is.na(city.mpg)) %>% 
  group_by(Make,Year) %>% 
  #srednie spalanie dla kazdej kombinacji marki z rokiem
  summarize(mean_city = mean(city.mpg)) %>% 
  #zmieniamy galony na litry 
  mutate(mean_L100km = 235.21 / mean_city) %>% 
  arrange(desc(mean_L100km))
  
  



# Odp.: najwiekse spalanie ma bugarri z 2008 i wynosi 29.4 litry/100km
# # Groups:   Make [48]
# Make         Year mean_city mean_L100km
# <chr>       <int>     <dbl>       <dbl>
#   1 Bugatti      2008       8          29.4
# 2 Bugatti      2009       8          29.4
# 3 Ferrari      2001       8.5        27.7
# 4 Lamborghini  2009       8.5        27.7
# 5 Lamborghini  2008       8.6        27.4
# 6 Ferrari      2005       9          26.1
# 7 Lamborghini  2001       9          26.1
# 8 Lamborghini  2010       9          26.1
# 9 Porsche      2004       9          26.1
# 10 Porsche      2005       9          26.1


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  


# Odp.:


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

zad8.1 <- data %>% 
  filter(!is.na(MSRP)) %>%
  # te wiersze, gdzie wartosi w kol dot ceny nie sa na
  filter(Year >= 200, Market.Category == "Luxury,Performance") %>%
  # te wiersze, gdzie kategoria to luxury, performance i wyprodukiowane po 2000 roku
  group_by(Make) %>%
  # grupowanie po producencie
  summarise(mediana_ceny = median(MSRP)) %>% 
  # liczymy mediane ceny dla kazdego producenta
  arrange(desc(mediana_ceny))

zad8.2 <- data %>% 
  filter(!is.na(Engine.Cylinders), !is.na(Engine.HP)) %>%
  # te wiersze, bez wartosci na
  filter(Year >= 200, Market.Category == "Luxury,Performance") %>%
  # te wiersze, gdzie kategoria to luxury, performance i wyprodukiowane po 2000 roku
  group_by(Make) %>% 
  # grupowanie po producencie
  summarise(srednia_cyl = mean(Engine.Cylinders), srednia_moc = mean(Engine.HP))


zad8.3 <- data %>% 
  #laczymy utworzone ramki wgledem prodycenta
  left_join(zad8.1, zad8.2, by = c("Make" = "Make")) %>% 
  arrange(desc(mediana_ceny)) %>% 
  #ustawiamy wzgl malejacych srednich cen
  slice(c(1,n()))


# Odp.: samochody Volkswagen Beetle Convertible mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok. 
#Srednia liczba cylindrów i mocy silnika marki wzrasta wzraz ze wzrostem mediany sugerowanej ceny detalicznej w tej kategorii.



# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

zad9 <- data %>% 
  filter(!is.na(city.mpg)) %>% 
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>%
  #te wiersze gdzie samochody sa z automatycza s.b. i w rozmiarze midzise
  #rosnaco
  arrange(city.mpg)

Q <- quantile(zad9$city.mpg, probs = c(0.1,0.9))

# Odp.:Q(0.1) = 14; Q(0.9) = 25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>% 
  filter(!is.na(Engine.HP), !is.na(Popularity)) %>%
  #wiersze bez wartosci na
  filter(Make == "Porsche", Engine.HP <= 300) %>% 
  #dla porsche bierzemy wiersze gdzie liczba km <= 300
  group_by(Model) %>% 
  summarise(popularnosc = sum(Popularity)) %>% 
  arrange(desc(popularnosc))
  

# Odp.: drugi najbardziej popularny spelniajacy warunki to model nr 944
# Model      popularnosc
# <chr>            <int>
# 1 968              10290
# 2 944               6860
# 3 Cayenne           6860
# 4 Boxster           5145
# 5 Cayman            5145
# 6 718 Cayman        1715
# 7 Cayman S          1715
# 8 Macan             1715
# > 


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
data %>% 
  filter(!is.na(Engine.HP)) %>%
  # wiersze bez na wartosci w kol dot koni mechanicznych
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  # grupujem po typie paliwa i rodzaju napedu
  summarise(medianahp = median(Engine.HP)) %>% 
  # mediana liczby koni mechanicznych
  arrange(desc(medaianahp))
# sortujemy malejaco po medianie liczby koni mechanicznych

# z głownej ramki danych wybieramy na podstawie poprzedniej analizy
# czyli wybieramy te rekordy dla ktorych mediana koni mechanicznych byla najwieksza
# wyznaczamy srednią spalania na autostradzie i korzystajac z przelicznika zamieniamy jednostke na L/100km
# zaokraglamy wynik do dwoch miejsc po przecinku
highway.MPG_L100 = round(235.21/mean(data[data$Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)" & 
                                            data$Driven_Wheels == "all wheel drive", "highway.MPG"]),5)


# Odp.: 12,2187


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>% 
  filter(!is.na(city.mpg), !is.na(highway.MPG)) %>%
  group_by(Make, Model) %>% 
  #grupujemy po marce i modelu
  summarise(srednia_miasta = mean(city.mpg), srednia_autostrada = mean(highway.MPG)) %>%
  mutate(srednia_miasta100 = 235.21/srednia_miasta, srednia_autostrada100 =235.21/srednia_autostrada) %>% 
  #zmieniamy przeliczniki
  mutate(roznica = abs(srednia_miasta100 - srednia_autostrada100)) %>% 
  arrange(desc(roznica))
  

# Odp.: ferrari enzo; srednia miasta w L/100 wynosi 33.6, srednia na autostradzie 19.6, czyli roznica w spalaniu wynosi wowczas 14.0 w L/100

