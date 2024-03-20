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
  summarise(median(Engine.HP, na.rm = TRUE))

# Odp.: 
# 1 AUTOMATED_MANUAL                                220
# 2 AUTOMATIC                                       253
# 3 DIRECT_DRIVE                                    147
# 4 MANUAL                                          172
# 5 UNKNOWN                                         125


# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?
x = data %>% 
  filter(Make == "Mercedes-Benz" & Engine.Fuel.Type == 'diesel') %>% 
  mutate(c.l.km = 235.214583 / city.mpg) %>% # co to w ogóle jest galon???
  arrange(c.l.km) %>% 
  select(c.l.km)

kwartyl_1 = x %>% 
  top_frac(-0.25) %>% 
  slice_tail(n=1)

kwartyl_3 = x %>% 
  top_frac(0.25) %>% 
  slice_head(n=1)

rozstep = kwartyl_3 - kwartyl_1
rozstep

# Odp.: Rozstęp międzykwartylowy wynosi 3.668064


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?

data %>% 
  filter(Engine.Cylinders == 4) %>% 
  select(Make, Model, MSRP) %>% 
  arrange(-MSRP) %>% 
  top_n(1, MSRP)

# Odp.: Lotus Exige, 74995


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
  
data %>% 
  filter(Make == "BMW") %>% 
  group_by(Driven_Wheels) %>% 
  summarise(n = n())

data %>% 
  filter(Make == "BMW", Driven_Wheels == "rear wheel drive") %>% 
  group_by(Model) %>% 
  summarise(n = n()) %>% 
  top_n(1)
# Odp.:
# 1 all wheel drive     144
# 2 front wheel drive     1
# 3 rear wheel drive    189
# Jak widać najwięcej jest samochodów BMW z napędem na tył. Najczęściej 
# występujący model z takim napędem to BMW serii 1 (16 samochodów).

# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>% 
  filter(Make == "Volkswagen") %>% 
  group_by(Number.of.Doors) %>% 
  summarise(median(Engine.HP))

# Odp.:
# 2                 170
# 4                 200
# Okazuje się, że mediana jest wyższa dla czterodrzwiowych Volkswagenów.


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>% 
  mutate(l.km = 235.214583 / city.mpg) %>%   # gotowy przelicznik z mpg na l/100km
  group_by(Make, Model, Year) %>% 
  arrange(-l.km) %>% 
  select(Make, Model, Year, l.km)
  
# Odp.:
# Pierwsza dziesiątka:
# 1 Ferrari     Enzo         2003  33.6
# 2 Ferrari     550          2001  29.4
# 3 Ferrari     550          2001  29.4
# 4 Lamborghini Murcielago   2008  29.4
# 5 Lamborghini Murcielago   2008  29.4
# 6 Lamborghini Murcielago   2009  29.4
# 7 Lamborghini Murcielago   2009  29.4
# 8 Bugatti     Veyron 16.4  2008  29.4
# 9 Bugatti     Veyron 16.4  2008  29.4
# 10 Bugatti     Veyron 16.4  2009  29.4
# Jak nietrudno się domyślić, są to samochody sportowe. Na pierwszym miejscu
# Ferrari Enzo z 2003 roku ze spalaniem 33.6 litra na 100km.


# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
  
x = data %>% 
  filter(Year >= 2007 & Year <= 2017) %>% 
  group_by(Year, Vehicle.Style) %>% 
  summarise(mean_pop = mean(Popularity)) %>% 
  group_by(Year) %>% 
  filter(mean_pop == max(mean_pop)) %>% 
  arrange(Year)

x %>%
  group_by(Vehicle.Style) %>% 
  summarise(n = n()) %>% 
  arrange(-n)


# Odp:
# 1  2007 Cargo Minivan          2964.
# 2  2008 Crew Cab Pickup        2461.
# 3  2009 Extended Cab Pickup    3583.
# 4  2010 Extended Cab Pickup    3093 
# 5  2011 Regular Cab Pickup     5657 
# 6  2012 Cargo Van              5657 
# 7  2012 Passenger Van          5657 
# 8  2013 Cargo Van              5657 
# 9  2013 Passenger Van          5657 
# 10  2014 Cargo Van              2858.
# Odpowiedź na pierwsze pytanie to powyższa tabela.
# 
# 1 Cargo Minivan           3
# 2 Cargo Van               3
# 3 Passenger Van           3
# 4 Extended Cab Pickup     2
# 5 Crew Cab Pickup         1
# 6 Regular Cab Pickup      1

# Odpowiedź na pytanie 2. to tabelka powyżej.

# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>% 
  filter(Year >= 2000 & Market.Category == "Luxury,Performance") %>% 
  group_by(Make) %>% 
  summarise(MSRP_med = median(MSRP, na.rm = TRUE)) %>% 
  arrange(-MSRP_med)

data %>% 
  filter(Year >= 2000 & Market.Category == "Luxury,Performance" & 
           (Make == "Volkswagen" | Make == "Hyundai")) %>% 
  group_by(Make) %>% 
  summarise(mean(Engine.Cylinders), mean(Engine.HP))
  
  
  
# Odp.:
# Najwyższe mediany:
# 1 Volkswagen       94600
# 2 Porsche          81400
# 3 Land Rover       67600
# 4 Audi             60100
# 5 Mercedes-Benz    55975
# 6 Cadillac         50050
# 7 BMW              48950
# 8 Lexus            48600
# 9 Infiniti         45700
# 10 Acura            45285
# 11 Genesis          42650
# 12 Saab             42000
# 13 Lincoln          39970
# 14 Volvo            39650
# 15 Hyundai          39625
# Zależności między marką, średnią liczbą cylindrów i mocą:
# 1 Hyundai                         6                 311
# 2 Volkswagen                     10.7               397

# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

data %>% 
  filter(Vehicle.Size == "Midsize" & Transmission.Type == "AUTOMATIC") %>% 
  mutate(c.l.km = 235.214583 / city.mpg) %>% # co to w ogóle jest galon???
  arrange(c.l.km) %>% 
  select(c.l.km) %>% 
  top_frac(-0.1) %>% 
  slice_tail(n=1)

data %>% 
  filter(Vehicle.Size == "Midsize" & Transmission.Type == "AUTOMATIC") %>% 
  mutate(c.l.km = 235.214583 / city.mpg) %>% # co to w ogóle jest galon???
  arrange(c.l.km) %>% 
  select(c.l.km) %>% 
  top_frac(0.1) %>% 
  slice_head(n= 1)
# Odp.: 0.1 kwantyl to 9.408583, a 0.9 kwantyl to 16.80104


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?

x = data %>% 
  filter(Make == "Porsche" & Engine.HP <= 300) %>% 
  group_by(Model) %>% 
  summarise(Popularity) %>% 
  arrange(-Popularity)

unique(x$Popularity)

# Odp.: Wychodzi na to, że wszystkie są równie popularne. 


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
data %>% 
  group_by(Engine.Fuel.Type, Driven_Wheels) %>% 
  summarise(mhp = median(Engine.HP, na.rm = TRUE)) %>% 
  arrange(-mhp)
data %>%
  filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)" & Driven_Wheels == "all wheel drive") %>% 
  mutate(h.l.km = 235.214583 / highway.MPG) %>% 
  summarise(mean(h.l.km))


# Odp.:
# 1 flex-fuel (premium unleaded required/E85)    all wheel drive   608 
# 2 flex-fuel (premium unleaded required/E85)    four wheel drive  510 
# 3 premium unleaded (recommended)               four wheel drive  420 
# 4 flex-fuel (premium unleaded recommended/E85) all wheel drive   403 
# 5 premium unleaded (required)                  rear wheel drive  386 
# 6 premium unleaded (required)                  all wheel drive   333 
# 7 premium unleaded (recommended)               rear wheel drive  330 
# 8 premium unleaded (required)                  four wheel drive  320.
# 9 flex-fuel (unleaded/E85)                     four wheel drive  310 
# 10 flex-fuel (unleaded/E85)                     all wheel drive   295 
# Widzimy, że jest to flex-fuel (premium unleaded required/E85) i all wheel drive;
# często spotykane połączenie w samochodach sportowych.
# Średnie spalanie na autostradzie wynosi 12.26251 L/100km.

# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?


data %>% 
  mutate(c.l.km = 235.214583 / city.mpg, h.l.km = 235.214583 / highway.MPG, hcd = abs(c.l.km - h.l.km) ) %>% 
  group_by(Make, Model) %>% 
  summarise(mhcd = max(hcd)) %>% 
  arrange(-mhcd)

# Odp.:
# Ta kombinacja to Ferrari Enzo. Różnica wynosi 14 L/100km.
# 1 Ferrari      Enzo            14.0
# 2 Bugatti      Veyron 16.4     12.6
# 3 Ferrari      575M            11.4
# 4 Ferrari      612 Scaglietti  11.4
# 5 Ferrari      Superamerica    11.4
# 6 Lamborghini  Murcielago      11.3
# 7 Aston Martin DB7             10.5
# 8 Bentley      Arnage          10.5
# 9 Bentley      Azure           10.5
# 10 Bentley      Azure T         10.5

