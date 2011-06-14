set.seed(6)
x <- c(rnorm(20), rnorm(20, mean = 0.75))
A <- gl(2, 20)

summary(aov(x ~ A))
summary(lm(x ~ A))

library(MCMCpack)
mcmcfit1 <- MCMCregress(x ~ A, burnin = 1000, mcmc = 10000, seed = 47)
plot(mcmcfit1)
summary(mcmcfit1)

mcmcfit2 <- MCMCregress(x ~ A, burnin = 1000, mcmc = 10000, seed = 83)
mcmcfit3 <- MCMCregress(x ~ A, burnin = 1000, mcmc = 10000, seed = 12)

mcmc.all <- mcmc.list(mcmcfit1, mcmcfit2, mcmcfit3)
gelman.diag(mcmc.all)
gelman.plot(mcmc.all)
