library(pscl)
library(AER)
library(lmtest)
library(sandwich)

set.seed(4)
n <- 1000
prop0 <- 0.4
y <- c(rpois((1-prop0)*n, 3), rep(0, times = prop0*n))
A <- factor(rep(c('AA', 'aa'), length = 1000))
B <- factor(c(rep(c('BB', 'bb'), length = (1-prop0)*n), 
              rep(c('BB', 'BB', 'BB','bb'), length = prop0*n)))

hist(y)

var(y[B == 'BB'])/
	mean(y[B == 'BB'])

fm_pois <- glm(y ~ A + B, family = poisson(log))
summary(fm_pois)
logLik(fm_pois)
AIC(fm_pois)
dispersiontest(fm_pois)

fm_nbin <- glm.nb(y ~ A * B, maxit = 1000)
summary(fm_nbin)
round(sum(dnbinom(0, mu = fitted(fm_nbin), size = fm_nbin$theta)))
logLik(fm_nbin)
AIC(fm_nbin)

fm_hurdle <- hurdle(y ~ A * B, dist = "negbin")
summary(fm_hurdle)
round(sum(predict(fm_hurdle, type = "prob")[,1]))
coeftest(fm_hurdle)
logLik(fm_hurdle)
AIC(fm_hurdle)

fm_zinb <- zeroinfl(y ~ A * B, dist = "negbin", link = 'log')
summary(fm_zinb)
AIC(fm_zinb)
round(sum(predict(fm_zinb, type = "prob")[,1]))
