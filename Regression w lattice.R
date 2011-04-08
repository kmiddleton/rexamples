library(lattice)
data(iris)
xyplot(Petal.Length~Petal.Width,
	groups=Species, type=c("p","smooth"), span=100, data=iris)
	
