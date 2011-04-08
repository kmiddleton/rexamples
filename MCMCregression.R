library(MCMCglmm)
set.seed(1)
x <- rnorm(100)
y <- 2 * x + rnorm(100)
df <- data.frame(x, y)

p.var <- var(x, na.rm = TRUE)
prior <- list(R = list(V = matrix(p.var/2), nu = 1), 
              G = list(G1 = list(V = matrix(p.var/2), nu = 1)))
model<-MCMCglmm(y ~ x, data = df, prior = prior,
  verbose = FALSE, nitt = 200000, thin = 10, burnin = 50000)

plot(model$VCV)
#summary(model$Sol)
posterior.mode(model$Sol)
HPDinterval(model$Sol, 0.95)

summary(lm(y ~ x, data = df))
