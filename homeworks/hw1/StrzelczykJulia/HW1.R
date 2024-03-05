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
# 1 AUTOMATED_MANUAL      220
# 2 AUTOMATIC             253
# 3 DIRECT_DRIVE          147
# 4 MANUAL                172
# 5 UNKNOWN               125

# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?

data %>% 
  filter(Make == "Mercedes-Benz" & Engine.Fuel.Type == "diesel") %>% 
  summarise(IQR = quantile(city.mpg,0.75)-quantile(city.mpg,0.25))

# Odp.: 7.25


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders == 4) %>% 
  group_by(Make) %>% 
  summarise(średnia_cena = mean(MSRP)) %>% 
  top_n(1,średnia_cena)

# Odp.: Alfa Romeo, 61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>% 
  filter(Make == "BMW") %>% 
  group_by(Driven_Wheels) %>% 
  summarise(n=n()) %>% 
  top_n(1,n)

data %>% 
  filter(Make == "BMW", Driven_Wheels == "rear wheel drive") %>% 
  group_by(Model) %>% 
  summarise(n=n()) %>% 
  top_n(1,n)

# Odp.: Najwięcej jest z napędem na tył. Najczęściej występujący model dla tego rodzaju napędu to 1 Series.


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make == "Volkswagen", Number.of.Doors == 2 | Number.of.Doors == 4) %>% 
  group_by(Number.of.Doors) %>% 
  summarise(mediana = median(Engine.HP))

# Odp.: 2 drzwi mediana - 170
#       4 drzwi mediana - 200


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>% 
  group_by(Year, Make) %>% 
  summarise(lkm = mean(3.785411784 * 100 / (city.mpg * 1.609344))) %>% 
  arrange(-lkm) %>% 
  head()

# Odp.: Bugatti z 2008 i 2009 roku. To spalanie wynosi 29.4 L/100km.


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
df1 <- data %>% 
  filter(Year>= 2007, Year <= 2017) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(srednia_popularnosc = mean(Popularity)) %>% 
  arrange(-srednia_popularnosc)

df2 <- df1 %>% 
  group_by(Year) %>% 
  summarise(max_popularnosc = max(srednia_popularnosc))

df <- df1 %>% 
  inner_join(df2, by = "Year") %>% 
  filter(srednia_popularnosc == max_popularnosc) %>% 
  arrange(Year)

df %>% 
  group_by(Vehicle.Style) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

# Odp.:
#    Year Vehicle.Style       srednia_popularnosc max_popularnosc
# 1  2007 Cargo Minivan                     2964.           2964.
# 2  2008 Crew Cab Pickup                   2461.           2461.
# 3  2009 Extended Cab Pickup               3583.           3583.
# 4  2010 Extended Cab Pickup               3093            3093 
# 5  2011 Regular Cab Pickup                5657            5657 
# 6  2012 Cargo Van                         5657            5657 
# 7  2012 Passenger Van                     5657            5657 
# 8  2013 Cargo Van                         5657            5657 
# 9  2013 Passenger Van                     5657            5657 
# 10  2014 Cargo Van                         2858.           2858.
# 11  2015 Cargo Minivan                     4337            4337 
# 12  2016 Cargo Minivan                     4051.           4051.
# 13  2017 Passenger Van                     5657            5657
# Jako średnio najbardziej popularny przez te 10 lat najczęściej wystąpił Cargo Minivan, Cargo Van i Passenger Van.


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
  filter(Market.Category == "Luxury,Performance", 2000<=Year) %>% 
  group_by(Make) %>% 
  summarise(mediana = median(MSRP)) %>% 
  arrange(-mediana) %>% 
  head()

data %>% 
  filter(Market.Category == "Luxury,Performance", 2000<=Year) %>% 
  group_by(Make) %>% 
  summarise(mediana = median(MSRP)) %>% 
  arrange(mediana) %>% 
  head()

data %>% 
  filter(Market.Category == "Luxury,Performance", Make == "Volkswagen" | Make == "Hyundai") %>% 
  group_by(Make) %>% 
  summarise(cylindry = mean(Engine.Cylinders), moc = mean(Engine.HP))


# Odp.: Samochody marki Volkswagen mają najwyższą medianę, a marki Hyundai najniższa,
#       różnica średniej liczby cylindrów - 4.7, różnica średniej mocy silnika - 86.


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>% 
  filter(Transmission.Type == "AUTOMATIC", Vehicle.Size == "Midsize") %>% 
  summarise(kwantyle = quantile(city.mpg, probs = c(0.1, 0.9)))

# Odp.: 0.1 kwantyl wynosi 14, a 0.9 kwantyl wynosi 25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

data %>% 
  filter(Make == "Porsche", Engine.HP <= 300) %>% 
  select(Model, Popularity)

# Odp.: Nie ma takiego, gdyż wszystkie modele są tak samo popularne.


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  summarise(km = median(Engine.HP, na.rm = TRUE)) %>% 
  arrange(-km) %>% 
  head()

data %>% 
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)", Driven_Wheels == "all wheel drive") %>% 
  summarise(lkm = mean(3.785411784 * 100 / (highway.MPG * 1.609344)))

# Odp.: Mediana koni mechanicznych jesy największa dla kombinacji flex-fuel (premium unleaded required/E85)
#       i napędu na cztery koła. Średnie spalanie wynosi 12.26251 L/100km


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

data %>% 
  group_by(Make, Model) %>% 
  summarise(roznica = abs(100 * 3.785411784/(1.609344*highway.MPG)-100 * 3.785411784/(1.609344*city.mpg))) %>%
  arrange(-roznica) %>% 
  head()

# Odp.: Ferrari Enzo. Ta różnica wynosi 14 L/100km.

