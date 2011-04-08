library(sfsmisc)

opl <- mult.fig(5, main= expression("Sine Functions " * sin(n * pi * x)))

x <- seq(0, 1, len = 201)

for (n in 1:5)
	plot(x, sin(n * pi * x), ylab ="", main = paste("n = ",n))

par(opl$old.par)


rr <- mult.fig(mfrow=c(5,1), main= "Cosinus Funktionen", cex = 1.5,
	marP = - c(0, 1, 2, 0))

for (n in 1:5)
	plot(x, cos(n * pi * x), type = 'l', col="red", ylab ="")

str(rr)

par(rr$old.par)

## The *restored* par settings:
str(do.call("par", as.list(names(rr$new.par))))
