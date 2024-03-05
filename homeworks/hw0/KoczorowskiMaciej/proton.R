install.packages("proton")
library(proton)
proton()

# ZADANIE 1
employees
employees[employees$name == "John" & employees$surname == "Insecure", "login"]
# login = johnins

# ZADANIE 2

length(top1000passwords)
 for (i in 1:1000) {
   proton(action = "login", login = "johnins", password = top1000passwords[i])
 }


# ZADANIE 3

employees[employees$surname == "Pietraszko", "login"]
# login = slap

logs2 <- logs[logs$login == "slap",]
host <- aggregate(logs2$login, list(logs2$host), length)
host2 <- host[order(host$x, decreasing = TRUE),]

proton(action = "server", host = as.character(host2[1, 1]))

# ZADANIE 4

splited <- unique(strsplit(bash_history, " ", fixed = TRUE))
"DHbb7QXppuHnaXGN"
splited[lapply(splited, length) == 1] # tylko 6 pozycji więc widać

#splitted bo przypisywanie do split nie działa :(