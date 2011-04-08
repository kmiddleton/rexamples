 library(MASS)
 mu <- c(0,1)
 Sigma <- matrix(c(1,.8,.8,1), ncol=2)
 set.seed(123)
 x <- mvrnorm(50,mu,Sigma)
 dta <- data.frame(x=x[,1],y=x[,2])
 plot(x)
 
 fit <- lm(y~x, data=dta)
 sfit <- summary(fit)
 se <- sfit$sigma
 abline(fit)
 
 yhat <- predict(fit,data.frame(x=c(-1,0,1)),se.fit=TRUE)
 
 x1 <- seq(-2,0,length=50)
 x2 <- x1 + 1
 x3 <- x2 + 1
 lines(x1,yhat$fit[1]+dnorm(x1,-1,se))
 lines(x2,yhat$fit[2]+dnorm(x2,0,se))
 lines(x3,yhat$fit[3]+dnorm(x3,1,se))