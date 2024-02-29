install.packages("proton")
library(proton)
proton()
#Pietraszko uses a password which is very difficult to guess.
#At first, try to hack an account of a person which is not as cautious as Pietraszko.

#But who is the weakest point? Initial investigation suggests that John Insecure doesn't care 
#about security and has an account on the Proton server. He may use a password which is easy to crack.
#Let's attack his account first!
  
#Problem 1: Find the login of John Insecure.

#Bit has scrapped 'employees' data (names and logins) from the www web page of Technical University of Warsaw. 
#The data is in the data.frame `employees`. 
#Now, your task is to find John Insecure's login.
#When you finally find out what John's login is, use `proton(action = "login", login="XYZ")` command, 
#where XYZ is Insecure's login.

employees
xyz <- employees[employees$surname == "Insecure",]$login
proton(action = "login", login=xyz)

#Congratulations! You have found out what John Insecure's login is!
#It is highly likely that he uses some typical password.
#Bit downloaded from the Internet a database with 1000 most commonly used passwords.
#You can find this database in the `top1000passwords` vector.

#Problem 2: Find John Insecure's password.

#Use `proton(action = "login", login="XYZ", password="ABC")` command in order to 
#log into the Proton server with the given credentials.
#If the password is correct, you will get the following message:
#`Success! User is logged in!`.
#Otherwise you will get:
#`Password or login is incorrect!`.

top1000passwords

for (i in 1:length(top1000passwords)) {
  if(proton(action = "login", login=xyz, password=top1000passwords[i]) == "Success! User is logged in!"){
    proton(action = "login", login=xyz, password=top1000passwords[i])
  }
}

#Well done! This is the right password!
#Bit used John Insecure's account in order to log into the Proton server.
#It turns out that John has access to server logs.
#Now, Bit wants to check from which workstation Pietraszko is frequently logging into the Proton server. 
#Bit hopes that there will be some useful data.  

#Logs are in the `logs` dataset. 
#Consecutive columns contain information such as: who, when and from which computer logged into Proton.

#Problem 3: Check from which server Pietraszko logs into the Proton server most often.

#Use `proton(action = "server", host="XYZ")` command in order to learn more  about what can be found on the XYZ server.
#The biggest chance to find something interesting is to find a server from which Pietraszko logs in the most often.

logs
log2 <- employees[employees$surname == "Pietraszko",]$login
tab2 <- table(logs$login)


proton(action = "server", host=host2)
