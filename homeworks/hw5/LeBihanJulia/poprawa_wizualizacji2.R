library(plotly)
library(dplyr)

podsektor <- c("Zboża", "Mleko krowie", "Hodowle wielorodzajowe", "Winogrona", "Mięso wołowe", "Hodowla owiec/kóz", 
               "Hodowla drobiu", "Hodowla krów jednocześnie mięsnych i mlecznych", "Hodowla świń", "Owoce", "Inne", "Zioła", 
               "Warzywa")

udział_w_konwencjonalnym <- c(29, 14.5, 13, 12, 8.5, 6, 3.4, 3.4, 2.4, 2.4, 2, 1.9, 1.5)

udział_w_bio <- c(13, 15.5, 11, 23, 7, 7, 5.5, 4, 1, 5, 2, 2, 4)

odsetek_bio <- c(1.8, 3, 2.3, 5.9, 2.2, 3.5, 4.2, 3.8, 1, 8, 2, 2, 11)

data <- data.frame(podsektor, udział_w_konwencjonalnym, udział_w_bio, odsetek_bio)


fig <- plot_ly(data, x = ~udział_w_konwencjonalnym, y = ~udział_w_bio, type = 'scatter', mode = 'markers', color = ~odsetek_bio, colors = c("#C1E1C1", "lightgreen", "#0eff00", "#1fc600", "#19A519", "#089000", "#0a5d00","#063b00"),
               marker = list(size=20, opacity = 0.9, sizemode = 'diameter'),
               text = ~paste('Podsektor:', podsektor, '<br>Jaki procent rolnictwa w tym podsektorze jest bio:', odsetek_bio, '%'))

fig <- fig %>% layout(title = 'Rolnictwo konwencjonalne i rolnictwo bio we Francji w 2013',
                      
                      xaxis = list(title ="Udział podsektora w całości rolnictwa konwencjonalnego (w %)",
                                   titlefont = list(size = 13), showgrid = FALSE),
                      
                      yaxis = list(title ="Udział podsektora w całości rolnictwa bio (w %)", 
                                   titlefont = list(size = 13),
                                   showgrid = FALSE),
                      paper_bgcolor = 'rgb(243, 243, 243)',
                      
                      plot_bgcolor = 'rgb(243, 243, 243)') %>% 
  colorbar(title = "Udział bio w podsektorze (w %)", titlefont = list(size = 11))

fig
