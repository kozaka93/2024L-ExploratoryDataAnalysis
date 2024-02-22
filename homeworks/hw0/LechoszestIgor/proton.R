install.packages("proton")
library(proton)
library(data.table)
proton()

df <- employees
df[df$surname == "Pietraszko",]

v <- top1000passwords

for (i in 1:1000) {
  if (proton(action = "login", login="johnins", password=v[i]) == "Success! User is logged in!") {
    print(v[i])
  } else {
  }
}

ds <- logs
fajnyds <- data.table(ds[ds$login == "slap", ])
fajnyds[, .N, by = host]

hist <- bash_history

for (i in 1:length(hist)) {
  if (length(strsplit(hist, " ")[i]) == 1) {
    proton(action = "login", login="slap", password=hist[i]) 
  } else {
  }
}

?strsplit




proton(action = "server", host="194.29.178.16")
proton(action = "login", login="slap", password="ABC")
proton(action = "login", login="johnins")
