library(lme4)

sleepsimfun1 <- function(b0, b1, Vb0, Vb1, V01, Serror) {
	mydata <- expand.grid( Days=0:9, Subject=1:18 )
	RE <- MASS::mvrnorm(18, c(0,0), matrix( c(Vb0,V01,V01,Vb1), 2) )
	mydata$Reaction <- with(mydata, 
		(b0+RE[Subject,1]) + (b1+RE[Subject,2])*Days + rnorm(180, 0, Serror)
			)

	fit1 <- lmer(Reaction ~ Days + (Days|Subject), mydata)
	fit2 <- lmer(Reaction ~ Days + (1|Subject) + (0+Days|Subject), mydata)
	anova(fit2,fit1)
}

We can use this to simulate data and analysis from the null condition (no correlation):

pb <- txtProgressBar(max=1000)
pbinc <- function() setTxtProgressBar(pb, getTxtProgressBar(pb)+1)

setTxtProgressBar(pb,0)
out1 <- replicate(1000, {pbinc(); 
		sleepsimfun1(251, 10.5, 627, 36, 0, sqrt(653))}, FALSE )

p1 <- sapply(out1, function(x) x[2,7])

hist( p1 )
mean( p1 <= 0.05 )
prop.test( sum(p1 <= 0.05), 1000 )