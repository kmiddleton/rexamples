# Using axTexpr

library(sfsmisc)

x <- 1e7*(-10:50)
y <- dnorm(x, m=10e7, s=20e7)
plot(x,y)## not really nice, the following is better:
## For horizontal y-axis labels, need more space:
op <- par(mar= .1+ c(5,5,4,1))
plot(x,y, axes= FALSE, frame=TRUE)
aX <- axTicks(1); axis(1, at=aX, label= axTexpr(1, aX))
## horizontal labels on y-axis:
aY <- axTicks(2); axis(2, at=aY, label= axTexpr(2, aY), las=2)
par(op)



## Now an `` engineer's version '' ( more ticks; only label "10 ^ k" ) :
axp <- par("xaxp") #-> powers of 10 *inside* 'usr'
axp[3] <- 1 # such that only 10^. are labeled
aX <- axTicks(1, axp = axp)
xu <- 10 ^ par("usr")[1:2]
e10 <- c(-1,1) + round(log10(axp[1:2])) ## exponents of 10 *outside* 'usr'
v <- c(outer(1:9, e10[1]:e10[2], function(x,E) x * 10 ^ E))
v <- v[xu[1] <= v & v <= xu[2]]
plot(x,y, xaxt= "n", log = "x", main = "engineer's version of x - axis")
axis(1, at = aX, label = axTexpr(1, aX, drop.1=TRUE)) # `default'
axis(1, at = v, label = FALSE, tcl = 2/3 * par("tcl"))
