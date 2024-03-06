install.packages("proton")
library(proton)
proton()
#zad1
employees
employees[employees$name == "John"& employees$surname == "Insecure", c("login")]
proton(action = "login", login="johnins")
#zad2
for (i in 1:1000){
  proton(action = "login", login="johnins", password=top1000passwords[i])
}
#zad3
employees[employees$surname == "Pietraszko", c("login")]



logi <- logs[logs$login == "slap", c("host")]
tabelka <- table(logi)
czesty_host <- as.data.frame(tabelka)
czesty_host[czesty_host$Freq == max(tabelka), c("logi")]

proton(action = "server", host="194.29.178.16")

kongo <- as.data.frame(bash_history)
?strsplit
