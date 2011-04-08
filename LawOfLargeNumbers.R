set.seed(1)

n <- 100000

x <- c(0, 1)
y <- sample(x, n, replace = TRUE)

means <- numeric(n)

for (i in 1:n)	means[i] <- mean(y[1:i])

plot(means, type = 'n', ylim = c(0, 1))
abline(h = 0.5, col = "red")
points(means, type = 'l')