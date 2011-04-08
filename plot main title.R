set.seed(42)
oldpar <- par(mfrow=c(1,2), mar=c(3,3,1,1), oma=c(0,0,3,1)) ## oma creates space 
plot(cumsum(rnorm(100)), type='l', main="Plot A")
plot(cumsum(rnorm(100)), type='l', main="Plot B")
mtext("Now isn't this random", side=3, line=1, outer=TRUE, cex=2, font=2)
par(oldpar)


par(mfrow=c(1,2), mar=c(3,3,1,1), oma=c(3,0,1,1)) ## oma creates space 
plot(cumsum(rnorm(100)), type='l', main="Plot A")
plot(cumsum(rnorm(100)), type='l', main="Plot B")
mtext("Now isn't this random", side=1, line=1, outer=TRUE, cex=2, font=2)
par(oldpar)
