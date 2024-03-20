install.packages("proton")
library(proton)
proton()

login1 <- employees[employees$name == "John" & employees$surname == "Insecure", "login"]
proton(action = "login", login=login1)

i <- 1
repeat {
  if (proton(action = "login", login=login1, password=top1000passwords[i]) ==  "Success! User is logged in!"){
    break
  }
  i <- i+1
}

pass1 <- top1000passwords[i]

login2 <- employees[employees$surname == "Pietraszko", "login"]

L <- as.data.frame(logs[logs$login == login2, "host"])
Tab <- as.data.frame(table(L))
host <- Tab[which.max(Tab$Freq),1]
proton(action = "server", host=as.character(host))

It turns out that Pietraszko often uses the public workstation 194.29.178.16.
What a carelessness.

Bit infiltrated this workstation easily. He downloaded `bash_history` file which contains a list of all commands that were entered into the server's console.
The chances are that some time ago Pietraszko typed a password into the console by mistake thinking that he was logging into the Proton server.

Problem 4: Find the Pietraszko's password.

In the `bash_history` dataset you will find all commands and parameters which have ever been entered.
Try to extract from this dataset only commands (only strings before space) and check whether one of them looks like a password.
