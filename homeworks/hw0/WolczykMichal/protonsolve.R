install.packages("proton")
library(proton)
proton()
head(employees)
df<- employees
df[df$surname == "Insecure","login"]
proton(action = "login", login="johnins")
for (i in 1:1000){
  A <- proton(action = "login", login="johnins", password=top1000passwords[i])
  if (A == "Success! User is logged in!"){print(top1000passwords[i])}
}
df[df$surname == "Pietraszko","login"]
logs[logs$login=="slap","host"]
