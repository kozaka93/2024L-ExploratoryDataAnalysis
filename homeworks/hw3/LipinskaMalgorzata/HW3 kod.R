
#załadowanie pakietów

library(ggplot2)
library(dplyr)

#przygotowanie danych do wykresu

procenty <- c(56 ,38, 53, 41, 50,44,49,46,42,53,41,56,39,57)
temat <- c('Climate change', 'Abortion', 'Election integrity', 'Health care', 'Foreign policy',
           'The economy', 'Immigration')
temat <- rep(temat, each=2)
kto <- c('Joe Biden', 'Donald Trump')
kto <- rep(kto, times=7)

#stworzenie ramki danych

data.frame(percents=procenty, topic=temat, Candidate=kto)->df

#stworzenie wykresu

df%>%
  ggplot(aes(x=topic, y=percents, fill=Candidate, label=paste0(percents, "%")))+
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9))+
  scale_fill_manual(values=c('red', 'blue'))+
  geom_text(position = position_dodge(width = 0.9), vjust = 1, size = 4, color = "white", aes(group = Candidate))+
  theme_bw()+
  labs(title='TRUST TO DO A BETTER JOB ON THE ISSUES',
       x='',y='')  +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  scale_x_discrete(position='top')+  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = -0.15),
        legend.position = 'top',
        legend.title = element_blank())







