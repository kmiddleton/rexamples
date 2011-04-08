library(MCMCpack)

line   <- list(X = c(-2,-1,0,1,2), Y = c(1,3,3,3,5))
posterior  <- MCMCregress(Y~X, data=line, verbose=0,
  burnin = 10000, mcmc = 100000, thin = 10)
plot(posterior)
summary(posterior)

lm(Y~X, line)
sd(line$X)