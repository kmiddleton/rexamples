breaks <- c(0, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150)
mids   <- c(10, 25, 35, 45, 55, 65, 75, 85, 95, 125)
counts <- c(87, 5, 2, 2, 1, 1, 0, 0, 1, 1)

library(stats4)

ll <- function(shape, rate)
{
     z <- pgamma(breaks, shape=shape, rate=rate)
     -sum(counts * log(diff(z)))
}

mle(ll, start=list(shape=1, rate=1/mean(breaks)))



x <- 0:10
y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)
ll <- function(ymax=15, xhalf=6)
    -sum(stats::dpois(y, lambda=ymax/(1+x/xhalf), log=TRUE))
(fit <- mle(ll))
mle(ll, fixed=list(xhalf=6))

summary(fit)
logLik(fit)
vcov(fit)
plot(profile(fit), absVal=FALSE)
confint(fit)