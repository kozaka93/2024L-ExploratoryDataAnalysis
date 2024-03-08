install.packages("proton")
library(proton)
proton()

i <- which(employees$name=='John' & employees$surname=='Insecure')
print(employees[i,])
proton(action = "login", login="johnins")

for  (i in 1:1000)
{
proton(action = "login", login="johnins", password=top1000passwords[i])
}

str(logs)
print(employees[employees$surname=="Pietraszko",])
names(which.max(table(logs[logs$login=='slap','host'])))
proton(action = 'server', host='194.29.178.16')


str(bash_history)
unique(gsub(" .*$", "",bash_history))
print(proton(action = "login", login="slap", password="DHbb7QXppuHnaXGN"))
