# Wizualizacja Śmiertelnych Wypadków Drogowych w UE

Ta interaktywna aplikacja Shiny zapewnia szczegółowe wizualizacje śmiertelnych wypadków drogowych w różnych krajach Unii Europejskiej. Wykorzystuje kilka zestawów danych, aby przedstawić kompleksowe spojrzenie na dane dotyczące wypadków na milion mieszkańców, segmentowane według demografii, rodzajów dróg i konkretnych lat.

## Funkcje

Aplikacja zawiera trzy główne zakładki:

### 1. Wizualizacja Mapy
- Wyświetla mapę geograficzną podkreślającą śmiertelne wypadki drogowe na milion mieszkańców dla każdego kraju w wybranym roku.
- Użytkownicy mogą wybrać rok z paska bocznego, co aktualizuje mapę odpowiednio.
- Najechanie na kraj wyświetla szczegółowe statystyki, takie jak nazwa kraju i liczba zgonów na milion mieszkańców.

### 2. Analiza Użytkowników Dróg
- Dostarcza informacji na temat demografii ofiar wypadków drogowych, podzielonych według wieku i płci dla wybranego kraju i roku.
- Dane są prezentowane na wykresach słupkowych pokazujących procent wypadków z udziałem różnych grup wiekowych i płci.

### 3. Analiza Typów Dróg
- Analizuje wypadki w zależności od typu drogi (autostrady, drogi wiejskie, drogi miejskie) dla wybranego kraju i roku.
- Wizualizacja pokazuje procent zgonów w stosunku do ogólnej liczby wypadków na każdym typie drogi.

## Źródła Danych

- **Dane o Wypadkach**: Zawiera całkowitą liczbę zgonów na milion mieszkańców, podzieloną według kraju i roku.
- **Dane Demograficzne**: Szczegóły dotyczące płci i grup wiekowych ofiar.
- **Dane o Typach Dróg**: Informacje o typach dróg, na których doszło do wypadków.

## Użycie

Aby uruchomić aplikację, upewnij się, że wszystkie wymagane biblioteki są zainstalowane i wykonaj skrypt R. Użyj opcji paska bocznego, aby filtrować dane według własnych preferencji (rok, kraj itp.) i eksplorować różne wizualizacje w zakładkach.

## Zależności

Aplikacja została zbudowana w języku R i wymaga następujących pakietów:

- Shiny
- ggplot2
- dplyr
- readr
- sf
- maps
- mapdata
- countrycode
- shinythemes
- plotly
