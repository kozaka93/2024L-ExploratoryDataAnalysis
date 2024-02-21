install.packages("proton")
library(proton)
proton()
employees[employees$name == 'John',]
proton(action = "login", login="johnins")
top1000passwords

for (i in 1:1000) {
  proton(action = "login", login="johnins", password = top1000passwords[i])
}

logs[,logs$login == 'john.insecure']
head(logs)
