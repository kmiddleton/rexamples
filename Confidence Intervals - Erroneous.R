# Neter p. XX
# Problem 19

setwd("/Users/kmm/Documents/Work/R/Neter Data Sets")

dat1<-read.table("/Users/kmm/Documents/Work/R/Neter Data Sets/CH01PR19.txt")

attach(dat1)

summary(dat1)

plot(dat1)

fit1 <- lm(V2 ~ V1)

summary(fit1)

abline(fit1)




set.seed(10)
# For example purposes, plot side by side
par(mfrow=c(1,2))

V1 <- rnorm(15)
V2 <- V1 + rnorm(15)

new <- data.frame(x = seq(min(V1), max(V1), length.out = length(V1)))

pred.w.clim <- predict(lm(V2 ~ V1), new, interval="prediction")
matplot(new$x, pred.w.clim,
        lty=c(1,2,2), type="l",  
        col=c("black", "red", "red"), 
        ylab="predicted y")
points(V1,V2)


# But if I create x & y equal to V1 & V2, matplot
x <- V1
y <- V2

pred.w.clim2 <- predict(lm(y ~ x), new, interval="prediction")
matplot(new$x, pred.w.clim2,
        lty=c(1,2,2), type="l",  
        col=c("black", "red", "red"), 
        ylab="predicted y")
points(x,y)

# Test if V1=x, V2=y
all.equal(x,V1)
all.equal(y,V2)

# Same output with predict
predict(lm(V2 ~ V1))
predict(lm(y ~ x))
all.equal(predict(lm(V2 ~ V1)), predict(lm(y ~ x)))

# Different output with interval="confidence"
pred.w.clim
pred.w.clim2
all.equal(pred.w.clim, pred.w.clim2)

detach(dat1)