tmp <- with(iris, seq(min(Petal.Length)-1, max(Petal.Length)+1, length.out=6))

with(iris, plot( Sepal.Width, Sepal.Length, 
	col = topo.colors(5)[cut(Petal.Length, tmp)]))
with(iris, lm(Sepal.Width ~ Sepal.Length))