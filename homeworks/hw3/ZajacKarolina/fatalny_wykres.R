################################################
###### KOMENTARZ DO ZAŁĄCZONEGO WYKRESU ########
################################################
# WYKRES JEST W FORMIE 3MIAROWEGO PIE, wartosci procentowe nie sumują sie do 100%, dlatego uznałam 
# brakujący procent jako kategorię inne, tzn wydatki_pozostale; z oficjalengo wykresu trudno
# rozpoznać kąty i wartości 



#dane:
wydatki_budzetu_panstwa <- c(92.43,7.5,0.07)
wydatki_budzetu_panstwa_nazwy <- c('wydatki pozostałe', 'rezerwy celowe', 'rezerwa ogólna')
rezerwy_celowe <- c(95.57,4.43)
nazwy <- c('rozdysponowanie_rezerw', 'pozostałość_środków')
rezerwa_ogólna <- c(72.42, 27.58)

# wykres:
par(mfrow = c(1, 3))

# Wykres dla wydatki_budzetu_panstwa
barplot(wydatki_budzetu_panstwa,
        names.arg = gsub("_", " ", wydatki_budzetu_panstwa_nazwy),  # Usuń podkreślenia
        main = "Procentowe wydatki budżetu państwa",
        xlab = "kategoria wydatków",
        ylab = "procentowy udział",
        col = c("skyblue", "yellow", "pink"),
        ylim = c(0, 100))
text(x = 1:length(wydatki_budzetu_panstwa),
     y = wydatki_budzetu_panstwa + 2,
     labels = paste(round(wydatki_budzetu_panstwa, 2), "%"),
     col = "black",
     cex = 0.8)



# Wykres dla rezerwy_celowej (żółty kolor)
barplot(rezerwy_celowe, 
        names.arg = gsub("_", " ", nazwy),  # Usuń podkreślenia
        main = "Procentowe rozdysponowanie rezerw celowych",
        xlab = "kategoria wydatków",
        ylab = "procentowy udział",
        col = "yellow",
        ylim = c(0, 100)) 
text(x = 1:length(rezerwy_celowe), 
     y = rezerwy_celowe + 2,  
     labels = paste(round(rezerwy_celowe, 2), "%"),  
     col = "black", 
     cex = 0.8)

# Wykres dla rezerwy_ogólnej (różowy kolor)
barplot(rezerwa_ogólna, 
        names.arg = gsub("_", " ", nazwy),  # Usuń podkreślenia
        main = "Procentowe pozostałość środków w rezerwie ogólnej",
        xlab = "kategoria wydatków",
        ylab = "procentowy udział",
        col = "pink",
        ylim = c(0, 100)) 
text(x = 1:length(rezerwa_ogólna), 
     y = rezerwa_ogólna + 2,  
     labels = paste(round(rezerwa_ogólna, 2), "%"),  
     col = "black", 
     cex = 0.8)

##komentarz do zapropowanego wykresu
# wykres słupkowy pomoga dokładniej dostrzec procetowy udział poszczególnych kategorii, kolory wprowadzają
# podkategorie; wartosci liczbowe nad slupkami pomagają dokładnie odczytać wartosci 



