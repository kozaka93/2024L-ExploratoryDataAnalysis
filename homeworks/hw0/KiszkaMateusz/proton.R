install.packages("proton")
library(proton)
library(data.table)
proton()
employees
#Pietraszko uses a password which is very difficult to guess.
#At first, try to hack an account of a person which is not as cautious as Pietraszko.

#But who is the weakest point? Initial investigation suggests that John Insecure doesn't care about security and has an account on the Proton server. He may use a password which is easy to crack.
#Let's attack his account first!
  
  #Problem 1: Find the login of John Insecure.

#Bit has scrapped 'employees' data (names and logins) from the www web page of Technical University of Warsaw. The data is in the data.frame `employees`. 
#Now, your task is to find John Insecure's login.
#When you finally find out what John's login is, use `proton(action = "login", login="XYZ")` command, where XYZ is Insecure's login.
employees[employees$surname == 'Insecure', 'login']
proton(action = "login", login="johnins")
top1000passwords[1]
for (i in 1:1000) {
  proton(action = "login", login="johnins", password=top1000passwords[i])
}
employees[employees$surname == 'Pietraszko', 'login']
logs
d <- data.table(logs[logs$login == "slap",])

d[,.N, by = "host"]

proton(action = "server", host="194.29.178.16")

bash_history
?strsplit
