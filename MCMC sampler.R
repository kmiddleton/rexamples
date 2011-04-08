MHastings <- function(n, p0, d){
	theta = vector(length = n)
	theta[1] <- p0
	t <- 1
	while(t <= n){
		phi <- log(theta[t]/(1-theta[t]))
		phisim <- phi + rnorm(1, 0, d)
		thetasim = exp(phisim)/(1+exp(phisim))
		r = (thetasim)^4*(1-thetasim)^8/(theta[t]^4*(1-theta[t])^8)
		if(runif(1,0,1)<r){
			theta[t+1] = thetasim
		} else {
			theta[t+1] = theta[t]
		}
		t = t+1
		if(t%%1000==0) print(t) # diagnostic
	}
	theta
}

hist(MHastings(10000, 0.4, 0.2))
