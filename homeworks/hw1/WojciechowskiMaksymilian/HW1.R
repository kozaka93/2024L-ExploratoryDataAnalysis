library(dplyr)

data <- read.csv("data.csv")
head(data, 10)
unique(data$Make)

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
  select(Engine.HP , Transmission.Type)%>% 
    group_by(Transmission.Type) %>% 
      summarise(mediana = median(Engine.HP, na.rm = TRUE))


# Odp.:
#   AUTOMATED_MANUAL      220
#   AUTOMATIC             253
#   DIRECT_DRIVE          147
#   MANUAL                172
#   UNKNOWN               125

# Zadanie 2 ---------------------------------------------------------------
# Jaki jest rozstęp międzykwartylowy spalania w mieście samochodów Mercedes-Benz,
# które korzystają z oleju napędowego (diesel)?
data %>% 
  filter(Make == "Mercedes-Benz" & Engine.Fuel.Type == "diesel") %>%
    summarise(iqr = IQR(city.mpg,na.rm = TRUE))

# Odp.: 7.25 


# Zadanie 3 ---------------------------------------------------------------
# Biorąc pod uwagę samochody z 4 cylindrowymi silnikami, który producent
# podaje najwyższą średnią sugerowaną cenę detaliczną? Ile wynosi ta cena?
data %>%
  filter(Engine.Cylinders == 4) %>%
    group_by(Make) %>%
      summarise(mean_p = mean(MSRP,na.rm = TRUE)) %>%
        arrange(desc(mean_p)) %>%
          head(1)
    


# Odp.: Alfa Romeo  61600


# Zadanie 4 ---------------------------------------------------------------
# Czy więcej jest Aut BMW z napędem na przód, na tył czy z napędem na 4 koła?
# Podaj najczęściej występujący model dla tego rodzaju napędu.
slim <- data %>%
  filter(Make == "BMW") %>%
    select(Driven_Wheels, Model) %>%
      group_by(Driven_Wheels, Model) %>%
        summarise(n = n())

slim %>% group_by(Driven_Wheels) %>% summarise(n = sum(n,na.rm = TRUE))
slim %>%
  filter(Driven_Wheels == "rear wheel drive") %>%
    arrange(desc(n)) %>% head(1)

# Odp.: Tył napęd, 1 series
# komentarz: miałem przeczucie że to będzie ta odp jeszcze zanim zajrzałem do ramki


# Zadanie 5 ---------------------------------------------------------------
# Jak zmienia się mediana mocy silnika w zależności od tego czy samochód posiada
# 2 lub 4 drzwi dla marki Volkswagen?

data %>%
  filter(Make == "Volkswagen" & Number.of.Doors == c(2, 4)) %>%
    group_by(Number.of.Doors) %>%
      summarise(mediana=median(Engine.HP,na.rm = TRUE))

# Odp.: Mediana dla 4 to 200, a 2 to 170. 


# Zadanie 6 ---------------------------------------------------------------
# Samochody, z którego roku i jakiej marki mają największe średnie spalanie 
# w mieście? Ile wynosi to spalanie? Wynik podaj w L/100km.

data %>%
  filter(Engine.Fuel.Type != "electric") %>%
    select(Make, Year, city.mpg) %>%
      group_by(Make, Year) %>%
        summarise(mean_c = mean(city.mpg, na.rm = TRUE)) %>%
          arrange(mean_c) %>%
            head(10) %>%
              mutate(spalanie = (1 / mean_c) * (378/1.609))

# Odp.: Bugatti rocznik 2008   29.4 L/100km

# Zadanie 7 ---------------------------------------------------------------
# Grupując po roku, w latach 2007-2017 jaki styl samochodów był średnio najbardziej popularny?
# Jaki styl najczęściej wystąpił jako średnio najbardziej popularny przez te 10 lat?
data %>%
  select(Year, Vehicle.Style, Popularity) %>%
    group_by(Year, Vehicle.Style) %>%
      filter(2007 <= Year & Year <= 2017) %>%
        summarise(mean_pop = mean(Popularity)) %>%
          group_by(Year) %>%
            filter(mean_pop == max(mean_pop))
# Odp.:
  #  2007 Cargo Minivan          2964.
  #  2008 Crew Cab Pickup        2461.
  #  2009 Extended Cab Pickup    3583.
  #  2010 Extended Cab Pickup    3093 
  #  2011 Regular Cab Pickup     5657 
  #  2012 Cargo Van              5657 
  #  2012 Passenger Van          5657 
  #  2013 Cargo Van              5657 
  #  2013 Passenger Van          5657 
  #  2014 Cargo Van              2858.
  #  2015 Cargo Minivan          4337 
  #  2016 Cargo Minivan          4051.
  #  2017 Passenger Van          5657
  
  # top Passenger Van / Cargo Minivan / Cargo Van 


# Zadanie 8 ---------------------------------------------------------------
# Jakiej marki samochody mają najwyższą medianę sugerowanej ceny w kategorii "Luxury,Performance"
# i są wyprodukowane po 2000 roku wliczając ten rok? Jak różni się średnia liczba cylindrów i mocy
# silnika marki z najwyższą i najniższą medianą sugerowanej ceny detalicznej w tej kategorii?

data %>%
  filter(Market.Category == "Luxury,Performance" & Year >= 2000) %>%
    group_by(Make) %>%
      summarise(med_price = median(MSRP)) %>%
        arrange(desc(med_price))



data %>%
  filter(  Market.Category == "Luxury,Performance" &
           Make == c("Volkswagen","Hyundai") &
           Year >= 2000) %>%
    group_by(Make)%>%
      summarise(mean_cyli = mean(Engine.Cylinders, na.rm = TRUE),
                mean_pow = mean(Engine.HP, na.rm = TRUE))




# Odp.:
#         Make            mean_cyli   mean_pow
#top      Volkswagen      11.2        408.
#bottom   Hyundai         6           311 


# Zadanie 9 ---------------------------------------------------------------
# Ile wynosi 0.1 i 0.9 kwantyl spalania w mieście dla samochodów
# z automatyczną skrzynią biegów i rozmiarze "Midsize"?

  x <- data %>% filter(Transmission.Type=="AUTOMATIC",Vehicle.Size=="Midsize")
  quantile(x$city.mpg,c(0.1, 0.9),na.rm = TRUE)

# Odp.: 0.1 -> 14, 0.9 -> 25


# Zadanie 10 --------------------------------------------------------------
# Jaki jest drugi najbardziej popularny model marki Porsche,
# który posiada nie więcej niż 300 koni mechanicznych?
  data %>%
    filter(Make=="Porsche",Engine.HP<=300) %>%
      group_by(Model) %>%
        summarise(pop = sum(Popularity)) %>%
          arrange(desc(pop))

# Odp.: 944 


# Zadanie 11 --------------------------------------------------------------
# Dla jakiej kombinacji typu paliwa i rodzaju napędu mediana koni mechanicznych jest największa?
# Ile wynosi średnie spalanie na autostradzie dla tej kombinacji w L/100?
  
  data %>%
    group_by(Engine.Fuel.Type, Driven_Wheels) %>%
      summarise(mediana = median(Engine.HP, na.rm = TRUE)) %>%
        arrange(desc(mediana)) %>%
          head(1)
  
  data %>%
    filter(Engine.Fuel.Type == "flex-fuel (premium unleaded required/E85)" & Driven_Wheels == "all wheel drive" ) %>%
      mutate(cons = (1 / highway.MPG) * (378/1.609)) %>%
          summarise(mead_cons = mean(cons,na.rm = TRUE))

# Odp.:flex-fuel (premium unleaded required/E85) all wheel drive, 12.2476l


# Zadanie 12 --------------------------------------------------------------
# Jaka kombinacja marki i modelu samochodu ma największą średnią różnicę
# spalania w mieście i na autostradzie w L/100? Ile wynosi ta różnica?

  data %>% filter(Engine.Fuel.Type != "electric") %>%
    mutate(spalanie_m = (1 / city.mpg) * (378/1.609),
           spalanie_a = (1 / highway.MPG) * (378/1.609)) %>%
      group_by(Model, Make) %>%
        summarise(mean_dif = mean(abs(spalanie_m - spalanie_a), na.rm = TRUE)) %>%
          arrange(desc(mean_dif))
    
# Odp.: Enzo           Ferrari        14.0 L

