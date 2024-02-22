install.packages("proton")
library(proton)
proton()
#johnins

#proton(action = "login", login="XYZ", password="ABC")

for (i in top1000passwords) {
  proton(action = "login", login="johnins", password=i)
}

print(top1000passwords)



wektor <- employees[employees$surname == 'Pietraszko',]

wektor2 <- logs[logs$login == 'slap',]
wektor2 <- data.frame(wektor2)

lapply(wektor2$host, median)

?strsplit
strsplit(wektor2$host, '.')
