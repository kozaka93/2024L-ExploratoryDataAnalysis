library(igraph)
library(tidygraph)
library(ggraph)
library(qgraph)
library(ggplot2)

g1 <- graph_from_data_frame(Sociogram1, directed = T)
g2 <- graph_from_data_frame(Sociogram2, directed = T)
g3 <- graph_from_data_frame(Sociogram3, directed = T)
g4 <- graph_from_data_frame(Sociogram4, directed = T)
g5 <- graph_from_data_frame(Sociogram5, directed = T)
g6 <- graph_from_data_frame(Sociogram6, directed = T)
g7 <- graph_from_data_frame(Sociogram7, directed = T)
g8 <- graph_from_data_frame(Sociogram8, directed = T)

LAYOUT <- function(g) {
  e <- get.edgelist(g, names = FALSE)
  qgraph.layout.fruchtermanreingold(
    e,
    vcount = vcount(g),
    area = 8 * (vcount(g) ^ 2),
    repulse.rad = (vcount(g) ^ 3.1),
    layout = 'spring'
  )
}

COLORS <- c(paste0(rep("grey", 19), seq(73, 1, by = -4)))

PLOT <- function(g, name) {
  plot.igraph(
    g,
    vertex.size = 12,
    vertex.color = COLORS[degree(g, mode = 'in') + 1],
    vertex.frame.color = 'grey3',
    vertex.frame.width = 1,
    vertex.shape = 'circle',
    vertex.label = NA,
    edge.color = ifelse(which_mutual(g), 'grey30', 'grey60'),
    edge.width = 2,
    edge.arrow.size = 0.7,
    layout = LAYOUT(g),
    rescale = T,
    asp = 0.8,
    frame = F,
  )
  title(toupper(paste('relationships between boys in',name)),cex.main=3.5)
  title(sub='Directed edge exists if the source wants to sit next to the target.\nThe darker the color of node, the higher its degree.\nColor of edge depends on mutuality - the darker color indicates the edge is mutual.',
adj=0.5,line=3.5, cex.sub=2.5)
}

PLOT(g1, 'grade 1')
PLOT(g2, 'grade 2')
PLOT(g3, 'grade 3')
PLOT(g4, 'grade 4')
PLOT(g5, 'grade 5')
PLOT(g6, 'grade 6')
PLOT(g7, 'grade 7')
PLOT(g8, 'grade 8')
