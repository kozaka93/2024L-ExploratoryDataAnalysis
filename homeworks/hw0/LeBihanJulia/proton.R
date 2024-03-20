install.packages("proton")
library(proton)
proton()

proton()
employees
employees[employees$surname == "Insecure", "login"]
proton(action = "login", login="johnins")
top1000passwords
for (i in 1:1000){
  itehaslo <- top1000passwords[i:i]
  if (proton(action = "login", login="johnins", password=itehaslo) == "`Success! User is logged in!`."){
    print(itehaslo)
    break
    
  }
}
employees[employees$surname == "Pietraszko", "login"]
logs[logs$login == "slap", ]