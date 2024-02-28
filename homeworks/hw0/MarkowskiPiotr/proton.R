install.packages("proton")
library(proton)
proton()


print(employees)
employees[employees$surname == "Insecure", "login"]


top1000passwords

for (i in 1:1000) {
  x <- top1000passwords[i]
  proton(action = "login", login="johnins", password=x)
}

logs[logs$login == "johnins", "host"]
