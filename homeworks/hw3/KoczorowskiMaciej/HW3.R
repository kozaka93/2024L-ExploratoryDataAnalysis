library(ggplot2)
library(dplyr)
library(forcats)

# Źródło wykresu:
# https://www.rp.pl/polityka/art40050711-sondaz-jak-polacy-oceniaja-zeznanie-jaroslawa-kaczynskiego-przed-komisja-ds-pegasusa
# Publikacja: 23.03.2024

# Wykres należałoby poprawić ponieważ:
# 1) Jest to wykres kołowy, co więcej wartości niektórych kategorii różnią się o niewielką
#    wartość, co jeszcze negatywniej wpływa na jego czytelność (wartości są podpisane - to
#    zdecydowanie zaleta, lecz nadal wykres mało czytelny)
# 2) Wartości nie sumują się nawet do 100%, najpewniej wynika to z błędu zaokrąglenia
# 3) Moim zdaniem niewiele sensu ma rozgraniczenie na osobne kategorie:
#    "Nie mam zdania" i "Nie interesują mnie prace tej komisji śledczej", bo ostatecznie
#    i tak osoba ankietowana nie miała zdania na dan temat, więc śmiało mogłoby to być w
#    jednej kategorii "Nie mam zdania"

df <- data.frame(cbind(c("Wypadł korzystnie", "Wypadł niekorzystnie", "Nie mam zdania"), c(19.4, 42.1, 37.5)))
colnames(df) <- c("Opinia", "Procent_respondentów")

ggplot(df, aes(x = factor(Opinia, levels = c("Wypadł korzystnie", "Wypadł niekorzystnie", "Nie mam zdania")), y = as.numeric(Procent_respondentów))) +
  geom_col(colour = "black", fill = "lightblue", linewidth = 0.7) +
  labs(title = "\"Jak wypadł Jarosław Kaczyński przed komisją ds. Pegasusa?\"",
       subtitle = "Na podstawie odpowiedzi respondentów", 
       y = "Procent ankietowanych", x = "Opinia")

# Nowy wykres jest lepszy od oryginalnego ponieważ na pierwszy rzut oka widoczne
# są różnice w wartościach, nie to co w przypadku wykresu kołowego. Ponadto przez
# zagregowanie dwóch w istotcie tożsamych kategorii widzimy, że zdecydowanie większy 
# odsetek ankietowanych nie ma żadnej opinii na temat poruszany przez ankietę, co
# pewnie chciano ukryć w gazecie. 