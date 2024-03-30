
# P1: Film, serial, książka, audiobook

Pierwszy projekt poświęcony jest eksploracji danych dotyczących filmów, seriali, książek, oraz audiobooków. Jego celem jest przygotowanie plakatu w formacie A2 (+ .pdf), który przedstawi graficznie ciekawe informacje.

Wykresy mogą być wykonane w dowolnym narzędziu i złożone w plakat z użyciem dowolnej techniki. Podczas *Wykładu <s>7</s> 8 i Projektu <s>7</s> 8 <s>(03-04-2024)</s> (10-04-2024)* zespoły przedstawiają krótkie prezentacje swojej pracy.

Plakat powinien składać się ze zbioru przynajmniej trzech spójnych tematycznie wykresów oraz komentarzy/opisów do wykresów. Projekt wykonuje się w zespole 3 osobowym. Kody źródłowe wykresów i plakat w postaci elektronicznej należy umieścić na GitHubie.

## Zajęcia projektowe

Zajęcia projektowe to głównie wspólne dyskusje, praca w grupie, prezentacje kolejnych etapów, konsultacje.

<table style="undefined;table-layout: fixed; width: 526px">
<colgroup>
<col style="width: 59.116667px">
<col style="width: 82.116667px">
<col style="width: 331.116667px">
<col style="width: 54.116667px">
</colgroup>
<thead>
  <tr>
    <th>Tydzień</th>
    <th>Data</th>
    <th>Zakres</th>
    <th>Punkty</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td>1</td>
    <td>21-02-2024</td>
    <td>Wprowadzenie do projektu, podział na zespoły.</td>
    <td></td>
  </tr>
  <tr>
    <td>2</td>
    <td>28-02-2024</td>
    <td>Praca w zespołach, burza mózgów, określenie tematyki plakatu, źródła danych</td>
    <td>1</td>
  </tr>
  <tr>
    <td>3</td>
    <td>06-03-2024</td>
    <td>Konsultacje: Problemy z danymi, zmiany tematyki, itp. </td>
    <td></td>
  </tr>
  <tr>
    <td>4</td>
    <td>13-03-2024</td>
    <td>Prezentacja wykonanej eksploracji danych, pierwsze wykresy i analizy </td>
    <td>2</td>
  </tr>
  <tr>
    <td>5</td>
    <td>20-03-2024</td>
    <td>Konsultacje: Prototypy wykresów, dyskusja nad układem plakatu </td>
    <td></td>
  </tr>
  <tr>
    <td>6</td>
    <td>27-03-2024</td>
    <td>Prezentacja zaawansowanych wizualizacji oraz prototypu plakatu. </td>
    <td>2</td>
  </tr>
 
</tbody>
</table>

## Ocena

Za projekt można otrzymać od 0 do 24 punktów, z czego:

-   5p (1 x 1p, 2 x 2p) uzyskuje się za przedstawienie postępu prac w danym tygodniu
-   5p uzyskuje się za przygotowanie estetycznych wykresów (dwa lub więcej)
-   5p uzyskuje się, jeżeli przygotowane wykresy mają wszystkie niezbędne elementy do poprawnego odczytania danych (tytuł, podtytuł, adnotacje na osiach, legenda, jednostki, opis jak czytać wykres)
-   5p uzyskuje się za estetykę i pomysłowość aranżacji wykresów i opisów w jedną całość
-   4p uzyskuje się za prezentację projektu

Podczas prezentacji z każdego zespołu musi być obecna co najmniej jedna osoba. Nieobecność całego zespołu podczas sesji plakatowej skutkuje brakiem punktów za prezentację (-4 pkt) oraz możliwością zdobycia maksymalnie 80% za plakat (12 pkt).

## Przykłady danych
-   https://imsdb.com - Baza danych z surowymi skryptami filmów - trzeba się trochę napracować aby zrobić z nich ramkę danych.
-   https://github.com/morethanbooks/projects/tree/master/LotR - Dane grafowe z Władcy Pierścieni
-   https://github.com/mathbeveridge/asoiaf - Dane grafowe z Pieśni Lodu i Ognia
-   https://github.com/srobz/How-I-Analyzed-Your-Mother - Projekt analizujący serial How I Met Your Mother
-   https://fangj.github.io/friends/ - Skrypty odcinków z Friends. Bardzo surowe, zbieranie danych będzie ciężkie.
-   https://www.kaggle.com/datasets/andrezaza/clapper-massive-rotten-tomatoes-movies-and-reviews - Zbiór danych o filmach z portalu Rotten Tomatoes.
-   https://www.kaggle.com/datasets/farrosalferro/rotten-tomatoes-top-movies - Zbiór danych o najlepszych filmach na podstawie portalu Rotten Tomatoes.
-   https://www.kaggle.com/datasets/asaniczka/amazon-kindle-books-dataset-2023-130k-books - Zbiór danych o książkach z Amazon Kindle.
-   https://www.kaggle.com/datasets/ashutoshdevpura/imdb-top-10000-movies-updated-august-2023 - Zbiór danych o filmach z serwisu IMDB.


**Uwagi**

-   Duża część danych na Kaggle'u to bardzo proste i małe zbiorki, z których być trudno będzie wyciągnąć coś ciekawego. Na pewno warto poświęcić trochę czasu na eksplorację większych zbiorów (lub też poszukać innych) i wybrać z nich pewien ciekawy do zwizualizowania podzbiór.
-   Niektóre zbiory danych nie są w plikach `.csv`. W przypadku problemów z odczytaniem danych warto zwrócić uwagę, czy na Kaggle'u lub w dokumentacji odpowiednich pakietów nie ma notatników/winietek, które mogą posłużyć za pomoc.

## Oddanie projektu

Czas na wykonanie projektu jest do <s>**02-04-2024**</s> **09-04-2024** - do tego dnia (włącznie) będą przyjmowane Pull Requests na GitHub.

W PR o nazwie `[projekt1] Nazwisko1_Nazwisko2_Nazwisko3` należy zamieścić folder o nazwie `nazwisko1_nazwisko2_nazwisko3` zawierający:

-   plakat w formacie .pdf o nazwie `nazwisko1_nazwisko2_nazwisko3`,
-   wszystkie kody służące do odtworzenia wykresów (na ile to możliwe) w podfolderze `kody`,
- plik README zawierający plakat, źródła danych, autorów.


Przykład poprawnego folderu [`nazwisko1_nazwisko2_nazwisko3`](https://github.com/kozaka93/LectureMaterials/tree/main/EDA/2023-2024/projects/project1/nazwisko1_nazwisko2_nazwisko3)

PR robi jedna osoba z zespołu. Folder należy umieścić w [../projects/project1](https://github.com/kozaka93/2024L-ExploratoryDataAnalysis).

Należy wydrukować plakat i przynieść go na Wykład <s>**7**</s> **8**. 

Uwagi:

-  na plakacie powinien znaleźć się podpis identyfikujący autorów oraz źródło/a danych

## Materiały

Przykłady narzędzi do tworzenia plakatów:

-   PowerPoint
-   [](https://www.canva.com/)[https://www.canva.com/](https://www.canva.com/) (Pro with [](https://education.github.com/pack)[https://education.github.com/pack](https://education.github.com/pack))
-   Inkscape

Plakaty z poprzednich lat:
- [A flavour of posters — posters about FOOD!](https://medium.com/@kozaka/a-flavour-of-posters-posters-about-food-2a1786c115dc)
- [Data Visualization Posters - Let the Music Speak!](https://medium.com/@kozaka/data-visualization-posters-let-the-music-speak-a52fbcda5687)
-  [Posters about sports enter the game!](https://medium.com/responsibleml/posters-about-sports-enter-the-game-4cd77e659afe)
-   [Posters that change the perspective on climate and the environment](https://medium.com/responsibleml/posters-that-change-the-perspective-on-climate-and-the-environment-c3682c0f6c39)
-   [poster::make([movie | book | series])](https://medium.com/responsibleml/poster-make-movie-book-series-3ac2c8a01180)
-   [COVID-19 Data Visualization](https://medium.com/responsibleml/covid-19-data-visualization-bc0732c19d46)
- [więcej...](https://github.com/MI2-Education/posters)
