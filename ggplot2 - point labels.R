library(ggplot2)
set.seed(4)
x <- 1:10
y <- rnorm(10)
d <- data.frame(x=x,y=y)

p <- qplot(x,y, d)

p + geom_text(data=subset(d, x == 10),
	mapping = aes(label = paste(x,round(y,2),sep=", ")), vjust = -0.5)
