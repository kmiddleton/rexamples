x <- c(6,11,5,14,30,11,17,3,9,3,8,8)
confint(lm(x~1), level=.9) 


library(Hmisc)
smean.cl.boot(x, conf.int=.9, B=10000)

# For binomial data
binconf(12, 50, method="all")
