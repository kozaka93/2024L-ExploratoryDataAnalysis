install.packages("proton")
library(proton)
proton()

employees <- employees
login <- employees$login[c(employees$name, employees$surname) == c("John", "Insecure")]

proton(action = "login", login= "johnins")

