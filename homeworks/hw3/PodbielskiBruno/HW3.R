library(dplyr)
library(ggplot2)

dane <- data_frame(c(31.9, 30, 10.7, 9.5, 8.6, 9.3)) %>%
  cbind(c("Koalicja Obywatelska", "PiS", "Trzecia Droga", "Lewica", "Konfederacja", "Niezdecydowani"))
colnames(dane) <- c("Procent", "Partia")
dane %>%
  ggplot(aes(x=reorder(Partia, 1:6), y=Procent)) + geom_col(fill="darkorange") +
  labs(title='Badanie poparcia do Sejmu IBRiS dla "Rzeczpospolitej"',
       subtitle="z 22-23 marca tego roku (metoda CATI, próba: 1067 osób)",
       x="Nazwa ugrupowania") + theme(axis.title=element_text(size=12)) +
  geom_text(aes(label=Procent), vjust=2, color="white", size=4.2)
