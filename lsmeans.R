set.seed(631)
n <- 100
dat <- data.frame(y=rnorm(n), A=factor(sample(1:2, n, replace=TRUE)),
                  B=factor(sample(1:2, n, replace=TRUE)),
                  C=factor(sample(1:2, n, replace=TRUE)),
                  d=rnorm(n))
fm <- lm(y ~ A + B + C + d, dat)
## Form a data frame of points to predict: all combinations of the
## three factors and the mean of the covariate.
p <- data.frame(expand.grid(A=1:2, B=1:2, C=1:2))
p[] <- lapply(p, factor)
p <- cbind(p, d=mean(dat$d))
p <- cbind(yhat=predict(fm, p), p)
## lsmeans for the three factors:
with(p, tapply(yhat, A, mean))
with(p, tapply(yhat, B, mean))
with(p, tapply(yhat, C, mean))

