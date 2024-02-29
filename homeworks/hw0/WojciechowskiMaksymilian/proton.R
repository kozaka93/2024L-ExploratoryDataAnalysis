# install.packages("proton")
# library(proton)
# proton()

df <- employees
df[df$name == "John",]
proton(action = "login", login = "johnins")

top1000passwords[1]
for (i in 1:length(top1000passwords)){
  proton(action = "login", login="johnins", password=top1000passwords[i])
  
}
