xpts <- seq(-3,3,.05)

# Here is my suggested setup for a two-column picture --
pdf("demo2.pdf", width=5.6, height=2.8, bg="cadetblue1", pointsize=8)
par(mai=c(.6,.6,.2,.2))
plot(xpts, sin(xpts*xpts), type="l", lwd=2, col="cadetblue4",
     xlab="x", ylab="sin(x*x)")
grid(col="white", lty=1, lwd=.2)
abline(h=0, v=0)

dev.off()

# My suggested setup for a square one-column picture --
pdf("demo1.pdf", width=2.8, height=2.8, bg="cadetblue1", pointsize=8)
par(mai=c(.6,.6,.2,.2))
plot(xpts, sin(xpts*xpts), type="l", lwd=2, col="cadetblue4",
     xlab="x", ylab="sin(x*x)")
grid(col="white", lty=1, lwd=.2)
abline(h=0, v=0)
dev.off()