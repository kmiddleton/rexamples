#postscript(width = 4.5, height = 6, pointsize = 10)
quartz(width = 4.5, height = 6)
par(mai = c(0.5, 0.25, 0.5, 0.25))
x = seq(-4, 4, by = .001)
y = gamma(x)
yr = 1/y
yr[is.na(y)] = 0
plot.new()
plot.window(xlim = c(-4, 4), xaxs = "i", 
            ylim = c(-5, 5), yaxs = "i")
abline(h = c(-(1:4), 1:4), col = "gray")
abline(v = c(-(1:3), 1:3), col = "gray")
abline(h = 0, v = 0, lwd = 1.25)
lines(x, y)
lines(x, yr, lty="11")
yoff = .075
xoff = .075
text(rep(0, 11) - xoff, -5:5 - yoff,
     -5:5, xpd = T, adj = c(1,1))
text(-4:4 - xoff, rep(0, 9) - yoff,
     -4:4, xpd = T, adj = c(1,1))
title("The Gamma Function and Its Reciprocal")
legend(2.5, -4, xjust = 0.5, yjust = 0.5,
       legend = c("Gamma", "Reciprocal"),
       lty = c("solid", "11"), bg = "white")
box()
#dev.off()
