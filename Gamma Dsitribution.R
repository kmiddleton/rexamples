plot(density(rgamma(100, shape = 2, rate = 1)), ylim=c(0, 0.5), xlim=c(0,10), type='n')

lines(density(rgamma(1000000, shape = 2, rate = 1)))