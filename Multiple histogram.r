# Multiple histogram

require(plotrix)

l <- list(rnorm(50),rnorm(50,sd=2),rnorm(50,mean=3))
multhist(l)