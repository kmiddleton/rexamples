set.seed(6)

N <- 10000
n <- 10
x <- rbinom(N,n,0.5)
phat <- sum(x)/(N*n)
dhat <- dbinom((0:n),n,phat)
Freq <- table(x)
Exp <- N*dhat
ChiSq <- sum(((Freq-Exp)^2)/Exp)
DF <- (n-1-1)
1-pchisq(ChiSq,DF)

library(vcd)
gf <- goodfit(x, type = "binomial",
  par = list(prob = 0.5, size = n))
summary(gf)

