x <- runif(100, -4, 4)
y <- 1 + 2 * x + rnorm(100, sd = 2)
fit <- lm(y ~ x)
 
plot(x, y)
abline(fit)
legend("topleft", expression(R[adj]^2 == 0.66))
