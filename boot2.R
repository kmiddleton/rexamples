library(boot)
library(car)
set.seed(671969)

df <- data.frame(Y = c(rnorm(100,0,1), rnorm(100,0,1)),
                 G = rep(c(1,2), each = 100))

boot.levene <- function(data,indices){
      levene.diff <- diff(tapply(df$Y[indices],
                            list(df$G[indices]), mad))
      return(levene.diff)
      }

first.try <- boot(df, boot.levene, 1000)