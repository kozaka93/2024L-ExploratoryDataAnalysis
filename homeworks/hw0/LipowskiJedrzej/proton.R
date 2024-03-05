install.packages("proton")
library(proton)
library(dplyr)
proton()
employees[employees$name == "John" & employees$surname == "Insecure", "login"]
proton(action = "login", login="johnins")

str(top1000passwords)

for (i in 1:length(top1000passwords)) {
  proton(action = "login", login="johnins", password=top1000passwords[i])
}

employees[employees$surname == "Pietraszko", "login"]

str(logs)

ips <- data.frame(table(logs[logs$login == "slap", "host"]))
ips[ips$Freq != 0,]

proton(action = "server", host="194.29.178.16")

str(bash_history)
commands <- data.frame(bash_history)
help(strsplit)

"bailey" "slap"

Bit infiltrated this workstation easily. He downloaded `bash_history` file which contains a list of all commands that were entered into the server's console.
The chances are that some time ago Pietraszko typed a password into the console by mistake thinking that he was logging into the Proton server.

Problem 4: Find the Pietraszko's password.

In the `bash_history` dataset you will find all commands and parameters which have ever been entered.
Try to extract from this dataset only commands (only strings before space) and check whether one of them looks like a password.