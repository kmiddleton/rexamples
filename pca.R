set.seed(7)
n <- 500
x1 <- rnorm(n)
x2 <- -1 * x1 + rnorm(n)

themin <- which(x2 == min(x2))
themax <- which(x2 == max(x2))

library(smatr)
ma <- line.cis(x2, x1, method = "MA")
ols <- line.cis(x2, x1, method = "OLS")


plot(x1, x2, asp = 1, pch = 16, cex = 0.5)

#MA
abline(coef = ma[, 1])
b <- -1/ma[2, 1]
abline(a = mean(x2) - b * mean(x1), b = b)

#OLS
#abline(coef = ols[, 1], lty = 2)
#b <- -1/ols[2, 1]
#abline(a = mean(x2) - b * mean(x1), b = b, lty = 2)
#legend("topleft", c("MA", "OLS"), lty = c(1, 2))

points(x1[themin], x2[themin], col = "blue", pch = 16)
points(x1[themax], x2[themax], col = "green", pch = 16)

#PCA
z <- prcomp(~ x1 + x2)

pc1 <- z$x[, 1]
pc2 <- z$x[, 2]
plot(pc1, pc2, asp = 1, pch = 16, cex = 0.5)
abline(v = 0)
abline(h = 0)
points(pc1[themin], pc2[themin], col = "blue", pch = 16)
points(pc1[themax], pc2[themax], col = "green", pch = 16)

