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

zadanie1 <- data %>% 
  filter(!is.na(Engine.HP)) %>%
    # filtrujemy rekordy bez braku danych w kolumnie Engine.HP
  group_by(Transmission.Type) %>% 
    # grupujemy rekordy po rodzaju skrzyni biegow
  summarise(med_engine_power = median(Engine.HP)) %>%
    # wyznaczamy mediane mocy silnika dla kolejnych rodzajow skrzyni biegow
  arrange(desc(med_engine_power))
    # sortujemy rekordy malejaco po medianie mocy silnika

# Odp.: Mediana mocy silnika w zaleznosci od skrzyni biegow:
# 1 AUTOMATIC                      253
# 2 AUTOMATED_MANUAL               220
# 3 MANUAL                         172
# 4 DIRECT_DRIVE                   147
# 5 UNKNOWN                        125

# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

zadanie2 <- data %>% 
  filter(Make == "Mercedes-Benz", Engine.Fuel.Type == 'diesel', !is.na('city.mpg')) %>%
    # wybieramy te rekordy dla ktorych producent to Mercedes-Benz, rodzaj paliwa do diesel
    # oraz te rekordy dla ktorych srednie zuzycie paliwa mowiace ile mil
    # mozna przejechac na jednym galonie paliwa w miescie nie ma braku wartosci
  arrange(city.mpg)
    # grupujemy rekordy rosnaco po srednim zuzyciu paliwa w miescie
Q <- quantile(zadanie2$city.mpg, probs = c(0.25,0.75)) 
  # korzystajac z funkcji quantile, wyznaczamy pierwszy i trzeci kwartylu
  # funkcja przyjmuje wekotr zmiennych, ktorych kwantyli szukamy oraz
  # kwantyle, ktorych wartosci potrzebujemy
IQR = Q[2]-Q[1]
  # wyznaczamy wartosc rozstepu kwartalowego odejmujac trzeci kwartyl od pierwszego

# Odp.: Rozstep kwartalowy wynosi 7.25

# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

zadanie3 <- data %>% 
  filter(Engine.Cylinders == 4, !is.na(MSRP)) %>%
    # wybieramy te rekordy, ktorych wartosc kolumny Engine.Cylinders wynosi 4 oraz te,
    # dla ktorych cena nie ma braku wartosci
  group_by(Make) %>% 
    # grupujemy rekordy po marce samochodu
  summarise(mean_price = mean(MSRP)) %>%
    # wyznaczamy srednia cene dla kazdej marki samochodu
  arrange(desc(mean_price))
    # sortujemy rekordy po sredniej cenie malejaco

# Odp.: Najwyzsza srednia sugerowana cene detaliczna ma Alfa Romeo i wynosi ona 61600

# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.

# najpierw sprawdzamy ktorych aut w zaleznosci od napedu jest wiecej
zadanie4a <- data %>% 
  filter(Make == "BMW") %>% 
    # wybieramy rekordy ktorych wartosc w kolumnie Make to BMW
  group_by(Driven_Wheels) %>% 
    # groupujemy rekordy po rodzaju napedy
  summarise(num = n()) %>% 
    # wyznaczamy ilosc rekordow w zaleznosci od rodzaju napedu
  arrange(desc(num))
    # sortujemy rekordy malejaco po liczbie rekordow

zadanie4b <- data %>% 
  filter(Make == "BMW", Driven_Wheels == "rear wheel drive") %>%
    # wybieramy samochody marki BMW z napedem na cztery kola
  group_by(Model) %>% 
    # grupujemy rekordy po modelu samochodu
  summarise(num = n()) %>% 
    # wyznaczamy liczbe rekordow dla kazdego modelu
  arrange(desc(num)) 
    # sortujemy malejaco po liczbie rekordow

# Odp.: 
# Najwiecej jest aut BMW z napedem na tyl, potem na 4 kola, a najmniej na przod
# Driven_Wheels         num
# 1 rear wheel drive    189
# 2 all wheel drive     144
# 3 front wheel drive     1
# Najczesciej wystepujacy model z napedem na cztery kola to 1 Series

# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

zadanie5 <- data %>% 
  filter(Make == "Volkswagen", !is.na(Engine.HP)) %>% 
    # wybieramy samochodu marki Volkswagen, ktorych moc silnika nie jest brakiem wartosci
  group_by(Number.of.Doors) %>% 
    # grupujemy rekordy po liczbe drzwi
  summarise(median_engine_power = median(Engine.HP)) %>% 
    # wyznaczamy mediane mocy silnika dla samochodow o tej samej liczbie drzwi
  filter(Number.of.Doors == 2 | Number.of.Doors == 4)
    # wybieramy te rekordy, dla ktorych liczba drzwi wynosi 2 lub 4

# Odp.: Mediana mocy silnika maleje wraz ze spadkiem ilosci drzwi w samochodzie:
#   Number.of.Doors median_engine_power
# 1               2                 170
# 2               4                 200

# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

zadanie6 <- data %>% 
  filter(!is.na(city.mpg)) %>%
    # wybieramy te rekordy dla ktorych wartosc sredniego spalania w miescie nie jest
    # brakiem wartosci
  group_by(Make, Year) %>% 
    # grupujemy rekordy po marce samochodu i roku produkcji
  summarise(mean_city.mpg = mean(city.mpg)) %>% 
    # wyznaczamy srednie spalanie w miescie dla kazdej unikatowej kombinacji roku i marki
  mutate(mean_L100km = 235.21 / mean_city.mpg) %>%
    # zamieniamy jednostki z mil/galon na L/100km korzystajac z przelicznika
  arrange(desc(mean_L100km))
    # sortujemy rekordy malejaco po sredniej wartosci zuzycia paliwa

# Odp.: najwieksze spalanie w miescie ma Bugatti wyprodukowane w 2008 roku
# srednie spalanie dla tych samochodow wynosi 29.40125 L/100 km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
# tworzymy pusta ramke danych, z trzema kolumnami: rok produkacji samochodu, styl i miara popularnosci
# kolumne Year uzupelniamy wartosciami z przedzialu 2007:2017, pozostale trzy pozostawiamy puste
zadanie7a <- data.frame(Year = seq(from = 2007, to = 2017, by = 1), 
                        Style = rep(NA, 11), 
                        Popularity = rep(NA, 11))
# korzystajac z petli for uzupelniamy ramke danych najbardziej popularnym samochodem z 
# danego roku i miara jego popularnosci
for (i in 1:11){
  x <- data %>% 
    filter(Year == i + 2006) %>% 
      # wybieramy interesujacy nas rok produkacji (z przedzialu 2007-2017)
    group_by(Vehicle.Style) %>% 
      # grupujemy rekordy po stylu samochodu
    summarise(pop = sum(Popularity)) %>%
      # sumujemy miare popularnosci dla kazdego modelu
    arrange(desc(pop))
      # sortujemy malejaco po sumie miary popularnosci
  # przepisujemy otrzymane dane do stworzonej przez nas ramki danych
  zadanie7a[i, 2] = x[1, 1]
  zadanie7a[i, 3] = x[1, 2]
}

zadanie7b <- zadanie7a %>% 
  group_by(Style) %>% 
    # grupujemy rekordy po stylu samochodu
  summarise(num = n()) %>%
    # wyznaczamy liczbe wystapien danego stylu
  arrange(desc(num))
    # sortujemy malejaco po liczbie wystapien


# Odp.: Średnio najbardziej popularny styl w latach 2007-2017
#    Year               Style Popularity
# 1  2007 Extended Cab Pickup      54885
# 2  2008     Crew Cab Pickup     113216
# 3  2009     Crew Cab Pickup     107887
# 4  2010             4dr SUV      78552
# 5  2011             4dr SUV      88240
# 6  2012       4dr Hatchback      55696
# 7  2013       4dr Hatchback      77932
# 8  2014             4dr SUV     144849
# 9  2015               Sedan     986057
# 10 2016               Sedan     998724
# 11 2017               Sedan     776483
# najczescie wystapily style 4dr SUV i Sedan z taka sama liczba wystapien: 3

# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

zadanie8a <- data %>% 
  filter(!is.na(MSRP)) %>%
    # wybieramy rekordy, ktore nie maja brakow danych w kolumnie cena
  filter(Year >= 200, Market.Category == "Luxury,Performance") %>%
    # wybieramy te rekordy samochodow wyprodukowane po 2000 roku, znajdujace sie w
    # kategorii "Luxury,Performance"
  group_by(Make) %>%
    # grupujemy rekordy po producencie
  summarise(median_price = median(MSRP)) %>% 
    # wyznaczamy wartosc mediany ceny dla kazdego producenta
  arrange(desc(median_price))
    # sortujemy rekordy malejaco po cenie

zadanie8b <- data %>% 
  filter(!is.na(Engine.Cylinders), !is.na(Engine.HP)) %>%
    # wybieramy te rekordy, ktore nie maja brakow wartosci dla kolumn z liczba cylindrow
    # i moca silnika
  filter(Year >= 200, Market.Category == "Luxury,Performance") %>%
    # wybieramy te rekordy samochodow wyprodukowane po 2000 roku, znajdujace sie w
    # kategorii "Luxury,Performance"
  group_by(Make) %>% 
    # grupujemy po producencie
  summarise(mean_cylinders = mean(Engine.Cylinders), mean_power = mean(Engine.HP))
    # wyznaczamy srednia liczbe cylindrow i srednia moc dla kazdego producenta

zadanie8c <- 
  left_join(zadanie8b, zadanie8a, by = c("Make" = "Make")) %>% 
    # laczymy ramki po producnecie samochodow
  arrange(desc(median_price)) %>% 
    # sortujemy malejaco po medianie ceny
  slice(c(1,n()))
    # wybieramy pierwsza i ostatnia obserwacje

# Odp.: Najwieksza mediane sredniej ceny dla samochodow wyprodukowanych po roku 2000
# w kategorii Luxury, Performance maja samochody marki Volkswagen.
# Na podstawie ramki danych zadanie8c mozemy zauwazyc, ze mediana ceny rosnie wraz ze
# wzrostem liczby cylindrow i mocy silnika


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

zadanie9 <- data %>% 
  filter(!is.na(city.mpg)) %>%
    # wybieramy te rekordy, ktorych wartosc dla sredniego spalania w miescie nie jest pusta 
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>%
    # wybieramy samochody z automatyczna skrzynia biegow i rozmiarze Midsize
  arrange(city.mpg)
    # sortujemy rosnaco po srednim spalaniu w miescie

Q <- quantile(zadanie9$city.mpg, probs = c(0.1,0.9)) 
  # do wyznaczenia kwantyli wykorzystujemy funkcje quantile

# Odp.: 0.1 kwantyl wynosi 14 mil na galon, a 0.9 kwantyl wynosi 25 mil na galon


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

zadanie10 <- data %>% 
  filter(!is.na(Engine.HP), !is.na(Popularity)) %>%
    # wybieramy te rekordy dla ktorych miara popularnosci i moc silnika nie maja 
    # brakow wartosci
  filter(Make == "Porsche", Engine.HP <= 300) %>% 
    # wybieramy samochody marki Porsche, ktorych moc nie przekracza 300 koni mechanicznych
  group_by(Model) %>% 
    # grupujemy po modelu
  summarise(pop = sum(Popularity)) %>% 
    # sumujemy miare popularnosci dla kazdego modelu
  arrange(desc(pop))
    # sortujemy malejaco po sumie miary popularnosci

# Odp.: Drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych to 944


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
zadanie11 <- data %>% 
  filter(!is.na(Engine.HP)) %>%
    # wybieramy te rekordy w kotrych nie ma brakow danych dla mocy silnika
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
    # grupujemy rekordy po typie paliwa i rodzaju napedu
  summarise(median_HP = median(Engine.HP)) %>% 
    # wyznaczamy mediane liczby koni mechanicznych
  arrange(desc(median_HP))
    # sortujemy malejaco po medianie liczby koni mechanicznych

# z głownej ramki danych wybieramy na podstawie poprzedniej analizy
# czyli wybieramy te rekordy dla ktorych mediana koni mechanicznych byla najwieksza
# wyznaczamy srednią spalania na autostradzie i korzystajac z przelicznika zamieniamy jednostke na L/100km
# zaokraglamy wynik do dwoch miejsc po przecinku
highway.MPG_L100 = round(235.21/mean(data[data$Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)" & 
                          data$Driven_Wheels == "all wheel drive", "highway.MPG"]),2)

# Odp.: Mediana koni mechanicznych jest najwieksza dla samochodow o nastepujacych parametrach
# typ paliwa: flex-fuel (premium unleaded required/E85)
# rodzaj napedu: all wheel drive
# Srednie spalanie dla tej kombinacji na autostradzie wynosi 12.22 L/100km


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

zadanie12 <- data %>% 
  filter(!is.na(city.mpg), !is.na(highway.MPG)) %>%
    # wybieramy te rekordy, dla ktorych wartosci sredniego spalania nie maja brakow danych
  group_by(Make, Model) %>% 
    # grupujemy rekordy po producencie i modelu
  summarise(mean_city = mean(city.mpg), mean_highway = mean(highway.MPG)) %>%
    # wyznaczamy srednie spalanie w miescie i na autostradzie dla kazdej kombinacji producenta i modelu
  mutate(mean_city_L100 = 235.21/mean_city, mean_highway_L100 =235.21/mean_highway) %>% 
    # zamieniamy jednostki na L/100km
  mutate(diff = abs(mean_city_L100 - mean_highway_L100)) %>% 
    # wyznaczamy roznice spalania
  arrange(desc(diff))
    # sortujemy malejaco po roznicy

# Odp.: Najwieksza roznice ma samochod marki Ferrari model Enzo