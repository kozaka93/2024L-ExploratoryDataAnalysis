install.packages("proton")
library(proton)
proton()

employees[employees$name == "John",]
for (i in 1:1000) {proton(action = "login", login="johnins", password=top1000passwords[i])}
table(factor(host$host))


It turns out that Pietraszko often uses the public workstation 194.29.178.16.
What a carelessness.

Bit infiltrated this workstation easily. He downloaded `bash_history` file which contains a list of all commands that were entered into the server's console.
The chances are that some time ago Pietraszko typed a password into the console by mistake thinking that he was logging into the Proton server.

Problem 4: Find the Pietraszko's password.

In the `bash_history` dataset you will find all commands and parameters which have ever been entered.
Try to extract from this dataset only commands (only strings before space) and check whether one of them looks like a password.