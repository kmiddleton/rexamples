# See: http://statistical-research.com/simulating-random-multivariate-correlated-data-categorical-variables/?utm_source=rss&utm_medium=rss&utm_campaign=simulating-random-multivariate-correlated-data-categorical-variables

library(GenOrd)
set.seed(1)

# Sets the marginals. The values are cumulative so for the first
# variable the first marginal will be .1, the second is .2, the third
# is .3, and the fourth is .4
marginal <- list(c(0.1,0.3,0.6),c(0.4,0.7,0.9))

# Checks the lower and upper bounds of the correlation coefficients.
corrcheck(marginal)

# Sets the correlation coefficients
R <- matrix(c(1,-0.6,-0.6,1),2,2) # Correlation matrix
n <- 100

# Selects and ordinal sample with given correlation R and given marginals.
m <- ordsample(n, marginal, R)

# Xompare it with the pre-defined R
cor(m)
table(m[,1],m[,2])

chisq.test(m)

gbar <- tapply(m[,1], list(m[,1], m[,2]), length)

par(mfrow=c(1,1))
barplot(gbar, 
        beside= TRUE, 
        col=cm.colors(4), 
        main="Example Bar Chart of Counts by Group",
        xlab="Group",
        ylab="Frequency")
