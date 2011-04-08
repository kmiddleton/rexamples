a <- rnorm(50)
b <- rnorm(50)+5

df1 <- data.frame(a, b)

paired <- function(x, i){
	out <- t.test(x$a[i], x$b[i], paired=T, mu=0)
	out$statistic}
	
booted <- boot(df1, paired, R=999)

length(which(booted$t <= booted$t0))