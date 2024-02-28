install.packages("proton")
library(proton)
proton()
employees[employees$surname=="Insecure",]
proton(action = "login",login="johnins")
top1000passwords
for (pass in top1000passwords) {
  proton(action = "login",login="johnins",passwords=pass)
}
aggregate(logs[logs$login=="slap",],by=list(logs[logs$login=="slap",]$host),FUN=length)
proton(action="server",host="194.29.178.16")
bash_history
