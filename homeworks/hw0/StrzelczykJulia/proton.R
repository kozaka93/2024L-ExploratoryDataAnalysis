install.packages("proton")
library(proton)
proton()
employees
employees$login[employees$name == "John" & employees$surname == "Insecure"]
proton(action = "login", login="johnins")
i <- 1
for (i in top1000passwords) {
  proton(action = "login", login="johnins", password=i)
}
logs
employees$login[employees$name == "Slawomir" & employees$surname == "Pietraszko"]
logs$host[logs$login == employees$login[employees$name == "Slawomir" & employees$surname == "Pietraszko"]]
