install.packages("proton")
library(proton)
proton()

head(employees)
employees[employees$surname == "Insecure",]
# login == johnins
proton(action = "login", login = "johnins")

for (i in 1:1000) {
  res = proton(action = "login", login = "johnins", password = top1000passwords[i])
  if (res == "Success! User is logged in!") {
    print(paste("Correct password:", top1000passwords[i]))
    break
  }
}
# password == q1w2e3r4t5

head(logs)

logs[logs$login == "johnins",] #TBD