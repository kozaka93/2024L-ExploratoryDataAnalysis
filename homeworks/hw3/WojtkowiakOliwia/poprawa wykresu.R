library(dplyr)
library(ggplot2)
library(forcats)
install.packages("forcats")
dane <- function() {
  partie <- c("Koalicja Obywatelska",
                  "PiS",
                  "     Trzecia droga     ",
                  "  Konfederacja",
                  "Lewica", "Inna odpowiedź    ")
  
  glosy <- c(29, 22, 15, 11, 7, 16)
  
  wyniki <- data.frame(partia = partie, poparcie = glosy)
  
  wyniki
}


obraz <- wyniki %>% 
    ggplot(aes(x = fct_reorder(partia, -glosy), y = poparcie)) +
    geom_col(fill = "blue", width  = 0.5) +
    theme_bw() +
    labs(title = "Poparcie dla poszczególnych partii politycznych",
         subtitle = "na podstawie badania CBOS przeprowadzonego w dniach 19-22 lutego",
         x = "Partia",
         y = "Poparcie w %") +
    geom_text(aes(label = paste0(round(poparcie, 0), "%")), vjust = -0.5) +
    theme(axis.text.x = element_text(size = 11, colour = "black"),
          plot.title = element_text(size = 20)) -> obraz
 
obraz

