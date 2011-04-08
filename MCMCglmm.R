library(ggplot2)
set.seed(10)
x <- rnorm(20)
y <- 2 * x + 2 + rnorm(20)

df1 <- data.frame(x, y)

qplot(x, y, df1, geom = c('point', 'smooth'), method = 'lm')

summary(fit1 <- lm(y ~ x))

library(MCMCglmm)
iter <- 100000
fit2 <- MCMCglmm(y ~ x, data = df1, nitt=iter, thin=10, burnin=(iter*0.25), verbose = FALSE)

plot(fit2$Sol)

posterior.mode(fit2$Sol)
HPDinterval(fit2$Sol, 0.95)
autocorr(fit2$Sol)