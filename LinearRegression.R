#Y is a (n x 1) vector of observed values
#X is an (n X 2) matrix of ones followed by observations
#β is a (2 X 1) vector of parameters

#β = (X'X)-1 X'Y

set.seed(1234)

x <- rnorm(20)
X <- matrix(c(rep(1, times = 20), x), ncol = 2)
y <- x + rnorm(20, sd = 0.5)

lm(y ~ x)

B <- (solve(t(X) %*% X)) %*% (t(X) %*% y)
B

#β = (X'IX)-1 X'IY
I.mat <- diag(length(x))
B2 <- (solve(t(X) %*% I.mat %*% X)) %*% (t(X) %*% I.mat %*% y)
B2
