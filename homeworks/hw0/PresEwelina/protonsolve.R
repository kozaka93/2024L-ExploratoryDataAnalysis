install.packages("proton")
library(proton)
proton()

head(employees)
log <- employees$login[employees$surname == "Insecure"]
print(log)
proton(action = "login", login = "johnins")
