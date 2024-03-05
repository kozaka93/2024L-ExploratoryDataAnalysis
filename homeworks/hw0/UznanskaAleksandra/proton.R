install.packages("proton")
library(proton)
proton()

head(employees)
proton(action = "login", login="XYZ")
employees[employees[,'name']=='John',]
print(employees[217,3])
