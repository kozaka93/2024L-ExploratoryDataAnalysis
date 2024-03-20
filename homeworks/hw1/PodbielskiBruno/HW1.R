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
  summarise(median=median(Engine.HP, na.rm=TRUE))

# Odp.: Rodzaj skrzyni      Mediana
#
#       AUTOMATED_MANUAL      220
#       AUTOMATIC             253
#       DIRECT_DRIVE          147
#       MANUAL                172
#       UNKNOWN               125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>%
  filter(Make=="Mercedes-Benz" & Engine.Fuel.Type=="diesel") %>%
  summarise(IQR=IQR(1 / (city.mpg * 1.609344 / (3.785411784 * 100))))

data %>%
  filter(Make=="Mercedes-Benz" & Engine.Fuel.Type=="diesel") %>%
  summarise(IQR=IQR(city.mpg))

# Spalanie interpretuję jako zużycie paliwa na danym odcinku drogi.
# Jednostki zostały zamienione na L/100km.
# W tym zadaniu i kolejnych przyjąłem przeliczniki galonów i mil jak w funkcji powyżej.
# 
# (*) Jeśli jednak spalanie to mpg, to od tego jest druga funkcja.
# Odp.: 3.395825 ewentualnie 7.25 (*)


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>%
  filter(Engine.Cylinders==4) %>%
  group_by(Make) %>%
  summarise(mean=mean(MSRP)) %>%
  top_n(1, mean)

# Odp.: Alfa Romeo 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>%
  filter(Make=="BMW") %>%
  group_by(Driven_Wheels) %>%
  summarise(n=n())

data %>%
  filter(Make=="BMW" & Driven_Wheels=="rear wheel drive") %>%
  group_by(Model) %>%
  summarise(n=n()) %>%
  arrange(-n)

# Odp.: Na tył: 189, najczęściej występujący model: 1 Series


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>%
  filter(Make=="Volkswagen") %>%
  group_by(Number.of.Doors) %>%
  filter(Number.of.Doors %in% c(2,4)) %>%
  summarise(median=median(Engine.HP))

# Odp.: Mediana rośnie
#
#       Drzwi Mediana
#         2     170
#         4     200


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>%
  group_by(Year, Make) %>%
  mutate(city.lpk = 1 / (city.mpg * 1.609344 / (3.785411784 * 100))) %>%
  summarise(mean=mean(city.lpk)) %>%
  arrange(-mean)

# Jako że w zad.2 była mowa o spalaniu (a nie średnim spalaniu), a tutaj chcemy
# mieć największe średnie spalanie, to interpretuję je jako największą średnią ze spalania.
# Odp.: Bugatti w 2008 i 2009 roku, 29.4 litry/100km


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
data %>%
  filter(Year >= 2007 & Year <= 2017) %>%
  group_by(Year, Vehicle.Style) %>%
  summarise(mean=mean(Popularity)) %>%
  slice_max(mean) %>%
  select(Year, Vehicle.Style)

data %>%
  filter(Year >= 2007 & Year <= 2017) %>%
  group_by(Year, Vehicle.Style) %>%
  summarise(mean=mean(Popularity)) %>%
  slice_max(mean) %>%
  group_by(Vehicle.Style) %>%
  summarise(n=n()) %>%
  top_n(1, n)

# Pierwszą część polecenia interpretuję jako wskazanie najbardziej popularnego stylu
# w danym roku. Drugą jako zliczenie ile razy dany styl był zwycięzcą.
# Odp.: 1.komenda daje odp.na pierwsze pytanie
#       Najbardziej popularne przez 10 lat: Cargo Minivan, Cargo Van, Passenger Van (po 3 razy)


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>%
  filter(Market.Category=="Luxury,Performance" & Year >= 2000) %>%
  group_by(Make) %>%
  summarise(median=median(MSRP), meanCyl=mean(Engine.Cylinders), meanHP=mean(Engine.HP)) %>%
  arrange(-median)

# Odp.: Najwyższa mediana sugerowanej ceny: Volkswagen  94600
#       Volkswagen ma średnio o 4,7 cylindra więcej niż Hyundai i o 86 więcej koni


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>%
  filter(Transmission.Type=="AUTOMATIC" & Vehicle.Size=="Midsize") %>%
  summarise(quantile=quantile(1 / (city.mpg * 1.609344 / (3.785411784 * 100)), probs=c(0.1, 0.9)))

data %>%
  filter(Transmission.Type=="AUTOMATIC" & Vehicle.Size=="Midsize") %>%
  summarise(quantile=quantile(city.mpg, probs=c(0.1, 0.9)))

# Odp.: Odpowiednio około 9,4 i 16,8 trzymając się wcześniejszej interpretacji
#       spalania. Przyjmując, że spalanie wyraża się jako mpg, mamy odpowiednio 14 i 25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>%
  filter(Make=="Porsche" & Engine.HP <= 300) %>%
  group_by(Model) %>%
  summarise(n=n()) %>%
  top_n(2, n) %>%
  slice_min(n)

# "Najbardziej popularny" interpretuję tutaj jako zliczenie wystąpień danego modelu,
# a nie mającego najwyższy wynik popularity (wtedy wszystkie mają 1715 - wystarczy
# po poleceniu filter dopisać View() a całą resztę obciąć i od razu widać).
# Odp.: 944 i Cayenne

# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>%
  mutate(highway.lpk = (3.785411784 * 100) / (highway.MPG * 1.609344)) %>%
  group_by(Engine.Fuel.Type, Driven_Wheels) %>%
  summarise(median=median(Engine.HP, na.rm=TRUE), mean=mean(highway.lpk)) %>%
  arrange(-median) %>%
  head(1)

# Odp.: Mediana jest największa dla kombinacji flex-fuel (premium unleaded required/E85)
#       all wheel drive. Średnie spalanie: 12.3 litra/100km


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>%
  mutate(diff.lpk = (abs(1/highway.MPG - 1/city.mpg) / 1.609344 * (3.785411784 * 100))) %>%
  group_by(Make, Model) %>%
  summarise(mean=mean(diff.lpk, na.rm = TRUE)) %>%
  arrange(-mean)

# Odp.: Ferrari Enzo  14 L/100km

