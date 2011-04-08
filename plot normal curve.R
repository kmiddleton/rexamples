xmin <- -5
xmax <- 5
ymin <- 0
ymax <- 0.5

mu <- 0
sd <- 1

xx <- seq(-5,5, length=100)
yy <- dnorm(xx,mu,sd)
plot(xx,yy,type='l', xlim=c(xmin,xmax), ylim=c(ymin,ymax),
	ylab='',xlab='x')
lines(c(mu,mu),c(par('usr')[3],dnorm(0,0,sd)), lty=2, col='blue')
lines(c(mu,mu+sd), dnorm(sd,0,sd)*c(1,1), lty=2, col='blue')

# OR #

plot(xx,yy,type='l', 
	ylab='',xlab='x')