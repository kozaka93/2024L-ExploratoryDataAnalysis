library(proton)
proton()
employees[employees$name == "John" & employees$surname == "Insecure", ]
proton(action = "login", login="johnins")

for (i in 1:1000){
  proton(action = "login", login="johnins", password=top1000passwords[i])
}

prob3 <- logs[logs$login == "johnins", ]
prob3
