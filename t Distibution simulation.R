set.seed(42)
mu <- 10
sigma <-  5
n <- 3
nsim <- 10000
m <- matrix(rnorm(n * nsim, mu, sigma), nsim, n)
t <- apply(m, 1, function(x) (mean(x) - mu)/(sd(x)/sqrt(n)))

library(lattice)
qqmath(t, distribution = function(x) qt(x, n - 1),
        panel = function(x, ...) {
          panel.qqmath(x, col = "darkblue", ...)
          panel.qqmathline(x, col = "darkred", ...)
        })
