x <- 1:10
z <- rnorm(10,0,3)
y <- x + z
my.df <- data.frame(x,y)


plot(x=my.df[,1], y=my.df[,2],
     xlab=expression(paste("Independent  (",mu,"g/L)")),
     ylab=expression(paste("Dependent (",mu,"g/L)")),
     family="serif")
