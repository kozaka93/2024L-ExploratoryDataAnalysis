install.packages("proton")
library(proton)
proton()

x <- employees
x[x$name == "John",]
proton(action = "login", login="johnins")