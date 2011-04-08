N  <- 50                   # number of participants
X1 <- rnorm(N, 175,  7)    # predictor 1
X2 <- rnorm(N,  30,  8)    # predictor 2
X3 <- rnorm(N,  60, 30)    # predictor 3
Y1 <-  0.2*X1 - 0.3*X2 - 0.2*X3 + rnorm(N, 0, 50)  # predicted variable 1
Y2 <- -0.1*X1 + 0.2*X2 + rnorm(N, 50)              # predicted variable 2
Y  <- cbind(Y1, Y2)        # predicted variables in multivariate form

# fit OLS regression, coefficients are identical to the two separate univariate fits
(lmFit <- lm(Y ~ X1 + X2 + X3))

# fit MANOVA and do several multivariate tests
manFit <- manova(lmFit)
summary(manFit, test="Hotelling-Lawley") # Hotelling-Lawley trace
summary(manFit, test="Pillai")           # Pillai-Bartlett trace
summary(manFit, test="Roy")              # Roy's largest root
summary(manFit, test="Wilks")            # Wilks' lambda

# compare to separate univariate regression analyses: different p-values
summary(lm(Y1 ~ X1 + X2 + X3))
summary(lm(Y2 ~ X1 + X2 + X3))
