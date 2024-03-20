install.packages("proton")
library(proton)
proton()

employees[employees$surname=="Pietraszko",]
proton(action="login", login="slap") #wrong :(

employees[employees$surname == "Insecure",]
proton(action="login", login="johnins")

proton(action="login", login="johnins", password="password")

for(i in 1:1000) {
  proton(action="login", login="johnins", password=top1000passwords[i])
}

y <- data.frame(table(logs[logs$login == "slap", "host"]))
y[y$Freq == max(y$Freq), "Var1"]

proton(action = "server", host="194.29.178.16")

one_word <- bash_history[grep(" ", bash_history, invert=TRUE)]
one_word[nchar(one_word) > 6]

proton(action = "login", login="slap", password="DHbb7QXppuHnaXGN")
