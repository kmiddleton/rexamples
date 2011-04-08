y <- rnorm(100, 10, 3)
A <- as.factor(rep(c(1,2), each=50))

lmfit <- lm(y ~ A)
rlmfit <- rlm(y ~ A)

coef(lmfit)
coef(rlmfit)

par(mfrow=(c(1, 2)))

hist(y)
hist(rlmfit$fitted.values)