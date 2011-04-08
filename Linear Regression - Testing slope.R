set.seed(4)
x <- runif(100, -3, 3)
y <- rnorm(100, 2 - 1 * x, 3)

plot(x, y)

############
fit0 <- lm(y ~ 1 + offset(-x))
fit1 <- lm(y ~ x)
anova(fit0, fit1)

library(car)
linear.hypothesis(fit1, c("x = -1"))
