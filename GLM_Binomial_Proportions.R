x <- seq(from=-1,to=1,length=41)
px <- exp(x)/(1+exp(x))
nn <- sample(8:12, 41, replace=TRUE)
yy <- rbinom(41, size=nn, prob=px)
y <- yy/nn
glm(y~x, family=binomial, weights=nn)

