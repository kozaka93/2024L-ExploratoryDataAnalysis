install.packages("proton")
library(proton)
proton()
employees
employees[employees$name == "John",]
proton(action = "login", login="johnins")
top1000passwords

for (i in 1:1000) {
  haslo <- top1000passwords[i]
  proton(action = "login", login="johnins", password= haslo)
} 

head(logs)
head(employees)

employees[employees$surname == "Pietraszko",]
#slap

logs[logs$login== "slap",] 



