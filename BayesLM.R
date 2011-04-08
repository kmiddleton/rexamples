chirps=c(20,16.0,19.8,18.4,17.1,15.5,14.7,17.1,15.4,16.2,15,17.2,16,17,14.1)
temp=c(88.6,71.6,93.3,84.3,80.6,75.2,69.7,82,69.4,83.3,78.6,82.6,80.6,83.5,76.3)

X <- cbind(1, temp)
m <- 1000000
s <- blinreg(chirps, X, m)

par(mfrow = c(1,2))
plot(density(s$beta[,1]), main = "Intercept")
plot(density(s$beta[,2]), main = "Slope")

mean(s$beta[,1])
mean(s$beta[,2])

(lmfit <- lm(chirps ~ temp))

