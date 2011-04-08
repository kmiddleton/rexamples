x <- rnorm(1000000)
hx <- hist(x, breaks=100, plot=FALSE)
plot(hx, col=ifelse(abs(hx$breaks) < 1.669, 4, 2))
