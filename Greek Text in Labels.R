theta <- 2.1

plot(NA, xlim=c(0,1), ylim=c(0,1), xlab=bquote(theta == .(theta)), 
	ylab=bquote(theta == .(theta)), 
	main=bquote(paste("Results for ",theta == .(theta))))

plot(1, xlab = expression(sigma[x]))