# # Lab 7 Wizualizacja grafów
#   
# Niniejszy notatnik powstał w ramach przedmiotu Wstęp do Eksploracji Danych, aby przybliżyć studentom sposoby wizualizacji grafów.
# 
# # Biblioteki

install.packages('tidygraph')
install.packages('ggraph')
library(igraph)
library(tidygraph)
library(ggraph)

# # Teoria grafów
# 
# ## Definicja grafu / sieci
# 
# Grafem nazwiemy parę $V, E$ - zbiór wierzchołków $V$ (vertices/nodes) oraz krawędzi $E$ (edges). W przypadku ich tworzenia oraz wizualizacji najważniejsze są właśnie krawędzie, gdyż nawet nie mając zbioru wierzchołków jesteśmy w stanie wygenerować go na podstawie zbioru krawędzi.
# 
# Za pomocą grafów możemy reprezentować wiele zjawisk, które bazują na relacjach między poszczególnymi obiektami.

# ## Rodzaje grafów
# 
# 1.  **Grafy proste** - krawędzie są niezorientowane, bez wag, istnieje tylko jedna warstwa, itd,
# 
# 2.  **Grafy skierowane** - krawędzie mają kierunek, np. istnieje krawędź z $A$ do $B$, ale z $B$ do $A$ już nie,
# 
# 3.  **Multigrafy** - dopuszczamy aby dwa wierzchołki łączyłą więcej niż jedna krawędź oraz pętle z $A$ do $A$,
# 
# 4.  **Grafy ważone** - każda krawędź ma przyporządkowaną wagę, co potem wpływa na interpretację sieci,
# 
# 5.  **Grafy warstwowe** - krawędzie należą do różnych warstw grafu, pomimo że zbiór wierzchołków jest stały,
# 
# 6.  **Hipergrafy** - najbardziej skomplikowane struktury zezwalające na nie-binarne relacje.
# 
# W trakcie laboratorium (a także w praktyce) najczęściej spotykamy się z grafami i kombinacjami z punktów 1-4.
# 
# ## Ważne pojęcia
# 
# 1.  **Macierz sąsiedztwa** - Macierz $A$ o wymiarach $N x N$ dla grafu o $N$ wierzchołkach, gdzie $a_{ij}$ to siła krawędzi z $i$ do $j$.
# 
# 2.  **Stopień wierzchołka** - Liczba krawędzi wychodzących z wierzchołka (grafy nieskierowane). Dla skierowanych mamy stopień wierchołka wchodzący (in) oraz wychodzący (out) i nie muszą być równe.
# 
# 3.  **Ścieżka w grafie** - Ścieżką między $A$ i $B$ nazwiemy zbiór krawędzi, który wspólnie łączą wierzchołki $A$ i $B$.
# 
# 4.  **Dystans** - Dystansem między $A$ i $B$; $d(A,B)$ nawiemy najkrótszą ścieżkę między $A$ i $B$.
# 
# 5.  **Spójność grafu** - graf jest spójny gdy z każdego wierzchołka istnieje ścieżka do każdego innego.
# 
# ## Sieci rzeczywsite (real networks)
# 
# W ramach laboratorium zajmować będziemy się jedynie sieciami rzeczywistymi, jako że są to struktury najczęściej występujące w naturze. Aby rozpoznać czy mamy do czynienia z taką siecią należy dokonać prostej analizy grafu i odpowiedzieć na następujące pytania:
#   
# 1.  Czy rozkład stopni wierzchołków ma gruby ogon?
# 2.  Czy mamy do czynienia ze zjawiskiem małego świata (średnica grafu mniejsza niż 6)? (small-world phenomenon)
# 3.  Czy krawęzie są silnie skorelowane? (clustering coefficient)
# 
# Jeśli na część z nich odpowiemy 'Tak' oznacza to że prawdopodbnie mamy do czynienia z siecią rzeczywistą.
# 
# # W jaki sposób można reprezentować grafy?
# 
# Istnieje wiele sposobów na przechowywanie struktur grafowych. Bardziej zaawansowane metody wykorzystują formaty takie jak np. gml, i trzymają wszystkie infromacje w jednym pliku.

?read.graph
dolphins_graph <- read_graph("data/dolphins.gml", format = "gml")
dolphins_graph

# Inne podejścia są natomiast bardziej prymitywne i wykorzystują formaty takie jak csv, aby w dwóch osobnych plikać trzymać informacje o wierzchołkach oraz krawędziach. W takich strukturach często zauważyć można dodatkowe informacje, takie jak np. waga krawędzi, albo dodatkowe cechy wierzchołków.

LOTR_edges <- read.csv('data/LOTR-edges.csv')
LOTR_nodes <- read.csv('data/LOTR-nodes.csv', sep = '\t')
View(LOTR_edges)
View(LOTR_nodes)

# W niektórych przypadkach, sieci (networks - inna nazwa na grafy) mogą być reprezentowane poprzez bardzo proste struktury, opisujące jedynie zbiór krawędzi.

Erdos_edges <- read.csv('data/ca-Erdos992.csv', sep = ' ', header = FALSE)
head(Erdos_edges)

# W przypadku danych rzeczywistych, najczęsciej spotkać można bardzo rozbudowane struktury, często zapisywane w formacie JSON, będące nieuporządkowanymi plikami ogromnych rozmiarów. Jako, że mamy tylko jedne laboratoria o grafach to skupimy się jedynie na wizualizacji dość prostych obiektów.
# 
# # Wizualizacja grafów
# 
# ## Igraph
# 
# igraph jest jednym z najpopularniejszych narzędzi służących do analizy grafów. Poza samą wizualizacją sieci, jest on przede szystkim zorientowany na ich analizę, oraz generowanie.
# 
# ### Dolphins

?igraph.plotting
?layout
plot.igraph(dolphins_graph)

# ### Zachary's karate club
# 
# Zadanie 1: analogicznie do zbioru dolphins, poprawcie graf przedstawiający relacje między członkami klubu Zacharego, tak aby wizualizacja była czytelniejsza.


karate <- read_graph("data/karate.gml", format = "gml")
plot.igraph(karate)

# ## GGraph
# 
# Alternatywą do wizualizacji grafów za pomocą biblioteki igraph jest inspirowany ggplotem, pakiet ggraph. Jest on zorientowany jedynie w kierunku wizualizacji i zasadniczo oferuje więcej możliwości niż sam igraph, jednak jest też trochę bardziej skomplikowany.
# 
# ### Dolphins
# 
# #### Wizualizacja

tg <- tidygraph::as_tbl_graph(dolphins_graph) %>% 
  tidygraph::activate(nodes)
tg

edge_list <- tg %>%
  activate(edges) %>%
  data.frame()

node_list <- tg %>%
  activate(nodes) %>%
  data.frame()

node_list$degree <- rep(0, nrow(node_list))
node_list$id <- node_list$id + 1

for (i in 1:nrow(node_list)) {
  node_list$degree[i] <- sum(edge_list$from == node_list$id[i]) + sum(edge_list$to == node_list$id[i])
}
head(edge_list)
head(node_list)

ig <- igraph::graph_from_data_frame(d = edge_list, vertices = node_list, directed = FALSE)

ig %>%
  ggraph(layout = "auto") +
  geom_node_point() +
  geom_edge_link() +
  geom_node_text(aes(label = label))


# #### Analiza grafowa
# 
# Aby uzupełnić naszą wiedzę na temat grafów, jako że policzyliśmy stopnie wierzchołków w grafie dokanmy także sprawdzenia czy sieć delfinów jest siecią rzeczywistą.

ggplot(node_list, aes(degree)) +         
  geom_histogram(bins = 10) +         
  labs(title = "Histogram of nodes degree (bin = 10)", x = "Wieghted node degree", y = "Number of nodes") +         
  theme_minimal()

# Z powyższego grafu wynika, że rozkład stopni wierzchołków nie do końca posiada gruby ogon oraz ma dość mało wierzchołków.

cat('Clustering coefficient:', transitivity(ig),'\nDiameter of the graph:', 
    diameter(ig, directed = FALSE, weights = NULL))

# Ponadto zobaczyć możemy, że funkcja transitivity (innna nazwa na clsutering coefficient, zakres wartości [0,1]) jest niewielka 0.31, natomiast średnica grafu (najdłuższa z najkrótszych ścieżek między wierchołkami) wynosi, aż 8.
# 
# Niniejsza sieć wykazuje zatem pewne cechy sieci rzeczywistej (clustering coefficient), natomiast nie jest ich zbyt wiele.
# 
# ### Zachary's karate club
# 
# Zadanie 2: analogicznie do zbioru dolphins, poprawcie graf przedstawiający relacje między członkami klubu Zacharego, tak aby wizualizacja była czytelniejsza. Dodatkowo dokonać analizy czy sieć jest rzeczywista czy nie.
# 
# #### Wizualizacja

tg <- tidygraph::as_tbl_graph(karate) %>% 
  tidygraph::activate(nodes)
edge_list <- tg %>%
  activate(edges) %>%
  data.frame()

node_list <- tg %>%
  activate(nodes) %>%
  data.frame()

node_list$degree <- rep(0, nrow(node_list))
node_list$id <- node_list$id

for (i in 1:nrow(node_list)) {
  node_list$degree[i] <- sum(edge_list$from == node_list$id[i]) + sum(edge_list$to == node_list$id[i])
  node_list$label[i]  <- node_list$id[i]
}
ig <- igraph::graph_from_data_frame(d = edge_list, vertices = node_list, directed = FALSE)

ig %>%
  ggraph() +
  geom_node_point() +
  geom_edge_link() +
  geom_node_text(aes(label = label))


# #### Analiza grafowa


