library("ape")
library("caper")
library("geiger")

# Set random number seed
set.seed(5)

# Generate random tree
randtree <- rcoal(20)

# Generate random data along tree, order same as tips in tree
randtrait <- rTraitCont(randtree)[randtree$tip.label]

# Make data frame for comparative.data
randData <- data.frame(randtrait, Species = names(randtrait))

# Make comparative data for pgls
CD <- comparative.data(randtree, randData, Species, vcv=TRUE, vcv.dim=3)

# Fit pgls as univariate model
fm <- pgls(randtrait ~ 1, data = CD, lambda = "ML")
summary(fm)
# lambda [ ML]  : 0.983
fm$aicc
#           [,1]
# [1,] -80.79802

#### Why doesn't this match AICc below? log Likelihoods are equal.
#### k must be different.

logLik(fm)
# 'log Lik.' 41.51012 (df=1)

fitContinuous(randtree, randtrait, model = "lambda")
# $opt
# $opt$lambda
# [1] 0.9826195
# $opt$aicc
# [1] -75.42025
# $opt$lnL
# [1] 41.51012
# $opt$k
# [1] 3

#### pgls() and fitContinuous() estimate lambda the same. This is
#### good. It means they are doing the same thing and any differences
#### with bivariate pgls come from the consideration of a second
#### variable.

#### Fit a white noise model with fitContinuous. This is non-phylogenetic.
fitContinuous(randtree, randtrait, model = "white")
# $opt$aicc
# [1] -44.68625

#### The lambda model has a much lower AICc, so it is very much better.

# Calculate phylogenetic signal (round to 3 decimal places)
round(phylosignal(randtraits, randtree), 3)
#     K PIC.variance.obs PIC.variance.rnd.mean PIC.variance.P PIC.variance.Z
# 0.526            0.009                 0.154          0.001         -2.515

#### K is < 1, which agrees with lambda being less than 1. Also the P-value
#### for K is 0.001, which agrees with the lambda model being much
#### preferred over the white noise model. There is significantly less
#### variation in the data than you would predict by brownian motion.
