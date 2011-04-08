library(MASS)
data(iris)
boxplot(iris$Sepal.Width ~ iris$Species)
quartz()
boxplot(iris$Sepal.Width ~ iris$Species, 
	at=rank(tapply(iris$Sepal.Width, iris$Species, median)))