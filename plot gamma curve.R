xmin <- -0.1
xmax <- 5
ymin <- -1
ymax <- 1

shape <- 1.5
rate <- 1
mean <- 1
sd <- 1

xx <- seq(xmin,xmax, length=500)

plot(xx,xx, xlim=c(xmin,xmax),ylim=c(ymin,ymax), xlab='x', ylab='y',type='n')

yyg <- dgamma(xx,shape,rate)
lines(xx,yyg, lwd=2)
lines(c(mean,mean),c(ymin,dgamma(mean,shape,rate)), lty=2)
lines(c(mean,mean+sd), dgamma(mean+sd, shape, rate)*c(1,1),
	lty=2)
            
            