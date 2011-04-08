x <- rnorm(15)
y <- x + rnorm(15)
predict(lm(y ~ x))
new <- data.frame(x = seq(-3, 3, length.out = length(x)))
predict(lm(y ~ x), new, se.fit = TRUE)
pred.w.plim <- predict(lm(y ~ x), new, 	
	interval="prediction")
pred.w.clim <- predict(lm(y ~ x), new, 
	interval="confidence")
matplot(new$x,cbind(pred.w.clim, pred.w.plim[,-1]),
        lty=c(1,2,2,3,3), type="l", ylab="predicted y")
        
matplot(new$x, pred.w.clim,
	lty=c(1,3,3), col = c("black", "red", "red"),type="l", 
		ylab="predicted y")
points(x,y)
