r <- 0.8

corrs <- vector(length = 7)

s <- c(10, 100, 10^3, 10^4, 10^5, 10^6, 10^7)
ind <- 1

for (n in s){
	x1 <- rnorm(n)
	x2 <- rnorm(n)
	y <- r * x1 + x2 * sqrt(1-r^2)
	corrs[ind] <- cor(x1, y)
	ind <- ind + 1
}

data.frame(n = s, corr = corrs)

plot(s, corrs, xlog = TRUE)


library(ecodist)
xy <- corgen(100, .8, epsilon=0.00001)
cor(xy$x, xy$y)