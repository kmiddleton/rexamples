set.seed(5)
n <- 1000
library(ggplot2)

dsmall <- diamonds[sample(nrow(diamonds), n), ]

qplot(log(carat), log(price), data=dsmall)

qplot(carat, x * y * z, data=dsmall)

qplot(log(carat), log(price), data=dsmall, color = cut)
qplot(log(carat), log(price), data=dsmall, size = cut)
qplot(log(carat), log(price), data=dsmall, shape = cut)

qplot(log(carat), log(price), data=dsmall, geom = "smooth")
qplot(log(carat), log(price), data=dsmall, geom = "quantile")
qplot(log(carat), log(price), data=dsmall, geom = "density2d")
qplot(log(carat), log(price), data=dsmall, geom = "boxplot")
qplot(log(carat), log(price), data=dsmall, geom = "path")
qplot(log(carat), log(price), data=dsmall, geom = "line")
qplot(carat, data=dsmall, geom = "histogram")
qplot(log(carat), data=dsmall, geom = "density")

qplot(carat, price, data=dsmall, geom=c("smooth", "point"))

# turn off CIs
qplot(carat, price, data=dsmall, geom=c("smooth", "point"), se=FALSE)

## Add regression lines
# Use LOESS
qplot(carat, price, data=dsmall, 
	geom=c("smooth", "point"), method = 'loess')

# Use lm
qplot(log(carat), log(price), data=dsmall, 
	geom=c("smooth", "point"), method = 'lm')

# Use rlm
qplot(log(carat), log(price), data=dsmall, 
	geom=c("smooth", "point"), method = 'rlm')

# Use a gam
library(mgcv)
qplot(log(carat), log(price), data=dsmall, 
	geom=c("smooth", "point"), method = 'gam', 
	formula = y ~ s(x))

# Subset out small diamonds 
dlittle <- subset(dsmall, carat < 2)

# Quantile plot
qplot(carat, price, data=dlittle, geom=c("point", "quantile"))

# 5% - 95% quantiles
qplot(carat, price, data=dlittle, 
	geom = c("point", "quantile"), 
	quantiles = seq(0.05, 0.95, 0.1))
	
# With density plot	
qplot(log(carat), log(price), data=diamonds, 
	geom = c("point", "density2d"))

# Semitransparent boxplot
qplot(color, price/carat, data=diamonds, geom="jitter", 
	colour=I(alpha("black", 1/100)))

# Histogram with fine bins
qplot(carat, data=diamonds, geom="histogram", 
	binwidth=0.01, xlim=c(0,3))

qplot(carat, data=diamonds, geom="density", 
	colour=color, size=I(1.5))
qplot(carat, data=diamonds, geom="histogram", fill=color)

# Timeseries
qplot(date, uempmed, data=economics, geom="line")
qplot(unemploy/pop, uempmed, data=economics, geom="path")
year <- function(x) as.POSIXlt(x)$year + 1900
qplot(unemploy/pop, uempmed, data=economics, 
	geom="path", size=year(date))

qplot(carat, data=diamonds, facets=color ~ ., 
	geom="histogram", binwidth=0.1, xlim=c(0, 3))
	
qplot(displ, hwy, data = mpg, color = factor(cyl))

p <- ggplot(mtcars, aes(x = mpg)) 
p <- p + layer( 
geom = "histogram", 
geom_params = list(fill = "steelblue"), 
stat = "bin", 
stat_params = list(binwidth = 2) 
) 
p 

qplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_smooth()

p <- ggplot(Oxboys, aes(x=Occasion, y=height)) + 
	geom_boxplot() 
p + geom_line(aes(group=Subject), colour="#3366FF")


# Chapter 4
# Note use of aes()
p <- ggplot(mtcars, aes(x = wt, y = mpg, colour = cyl))
# Add a layer
p <- p + layer(geom = "point") 

p <- ggplot(mtcars, aes(x = mpg)) 
p <- p + layer( 
	geom = "histogram", 
	geom_params = list(fill = "steelblue"), 
	stat = "bin", 
	stat_params = list(binwidth = 1) 
) 
p

bestfit <- geom_smooth(method = "lm") 
qplot(mpg, wt, data = mtcars) + bestfit

# Updating
p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom point() 
p 
mtcars <- transform(mtcars, mpg = mpg ^ 2)
p %+% mtcars


data(Oxboys, package = 'nlme')
p <- ggplot(Oxboys, aes(age, height, group = Subject)) + geom_line() 
p
p + geom_smooth(aes(group = 1), method="lm", size = 2)

ggplot(diamonds, aes(x=carat)) + geom_histogram(aes(y=..density..), binwidth=.1)


# Chapter 5
qplot(depth, ..density.., data = diamonds, geom = "histogram", xlim = c(58, 68), binwidth = 0.1, facets = cut ~ ., fill = cut)

