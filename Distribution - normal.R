x <- seq(-4,4,.1)
plot(x,dnorm(x),type="n",xaxs="i",yaxs="i",ylim=c(0,.4),bty="l",xaxt="n")

polysection <- function(a,b,dist=dnorm,col="blue",n=11){
    dx <- seq(a,b,length.out=n)
    polygon(c(a,dx,b),c(0,dist(dx),0),border=NA,col=col)
}

library(RColorBrewer)
cols<-rev(brewer.pal(4,"Blues"))
for(i in 0:3){
    polysection(i,i+1,col=cols[i+1])
    polysection(-i-1,-i,col=cols[i+1])
}

sx <- -3:3
segments(sx,0,sx,dnorm(sx),col="white")
lines(x,dnorm(x))
axis(1,sx,sx)