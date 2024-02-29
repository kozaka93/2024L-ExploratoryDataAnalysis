install.packages("proton")
library(proton)
proton()
head(employees)
employees[employees$name == "John" && employees$surname == "Insecure"]


