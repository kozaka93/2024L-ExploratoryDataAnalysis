library(dplyr)
library(stringr)
library(ggplot2)
library(vioplot)

data <- read.csv("data/HIMYM_p.csv")

father_data <- data %>%
  filter(str_detect(tolower(Line), " father| papa| daddy| pops| old man| dad")) %>%
  filter(Season < 7) %>%
  distinct(Line, .keep_all = TRUE) %>%
  mutate(motive_f = "Father")


mother_data <- data %>%
  filter(str_detect(tolower(Line),
                    " mom | mom's | mom, | mom. | mom? | mom! | mommy | mum")) %>%
  filter(Season < 7) %>%
  distinct(Line, .keep_all = T) %>%  
  mutate(motive_m = "Mother")


# probny wykres, bo bez niego kod linijke nizej sie nie odpali
vioplot(
  Episode ~ Season,
  data = mother_data,
  ylim = c(0,30)
  )
# tlo
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = rgb(94 / 255, 120 / 255, 155 / 255), border = NA)
# prawa violin
vioplot(
  Episode ~ Season,
  data = mother_data,
  col = "violet",
  plotCentre = "line",
  side = "right",
  drawRect = F,
  ylim = c(0, 30),
  axes = F,
  add = T
)
#lewa violin
vioplot(
  Episode ~ Season,
  data = father_data,
  col = "yellow",
  plotCentre = "line",
  side = "left",
  add = TRUE,
  drawRect = F,
  ylim = c(0, 30),
  axes = F
)
# techniczne kolorki
axis(side = 1, col.axis = "yellow",
     col = "yellow")  # Oś x
axis(side = 2, col.axis = "yellow", 
     col = "yellow")  # Oś y
# legenda
legend(1,30,
       fill = c("violet", "yellow"),
       legend = c("mother", "father"),
       title = "Appearance of words like \n 'father' and 'mother'",
       bty = "n",
       cex = 0.9,  
       text.col = "white"
)



#wiemy, ze osie nie sa czytelne. Powyzszy kod został stworzony do wykresu. Poniżej znajduje się kod tworzący czytelny wykres

# tlo
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = rgb(94 / 255, 120 / 255, 155 / 255), border = NA)
# prawa violin
vioplot(
  Episode ~ Season,
  data = mother_data,
  col = "violet",
  plotCentre = "line",
  side = "right",
  drawRect = F,
  ylim = c(0, 30),
  add = T
)
#lewa violin
vioplot(
  Episode ~ Season,
  data = father_data,
  col = "yellow",
  plotCentre = "line",
  side = "left",
  add = TRUE,
  drawRect = F,
  ylim = c(0, 30)
)
# techniczne kolorki
axis(side = 1, col.axis = "black",
     col = "black")  # Oś x

axis(side = 2, col.axis = "black", 
     col = "black")  # Oś y
# legenda
legend(1,30,
       fill = c("violet", "yellow"),
       legend = c("mother", "father"),
       title = "Appearance of words like \n 'father' and 'mother'",
       bty = "n",
       cex = 0.9,  
       text.col = "white"
)




