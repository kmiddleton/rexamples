set.seed(1)

df <- data.frame(x = rnorm(1000), y = rnorm(1000), A = factor(rep(c("A", "a"), each = 500)))

mean(df$x[df$A == "A"])
mean(df$x[df$A == "a"])

mean(df$y[df$A == "A"])
mean(df$y[df$A == "a"])

library(plyr)
ddply(df, c("A"), function(df)c(mean(df$x), mean(df$y)))
