mcmcpvalue <- function(samp)
{
   ## elementary version that creates an empirical p-value for the
   ## hypothesis that the columns of samp have mean zero versus a
   ## general multivariate distribution with elliptical contours.

   ## differences from the mean standardized by the observed
   ## variance-covariance factor
   std <- backsolve(chol(var(samp)),
                    cbind(0, t(samp)) - colMeans(samp),
                    transpose = TRUE)
   sqdist <- colSums(std * std)
   sum(sqdist[-1] > sqdist[1])/nrow(samp)
}

#At least I think I have the standardization by the Cholesky factor of
#the observed variance-covariance matrix correct. However I always
#manage to confuse myself on that calculation so please let me know if
#I have it wrong.

#As an example, consider a model fit to the AvgDailyGain data from the
#SASmixed package.

#Fitting the same model in lmer then generating an MCMC sample and
#testing for the three interaction coefficients being zero would look
#like

data(AvgDailyGain, package = "SASmixed")
(fm1Adg <- lmer(adg ~ InitWt*Treatment - 1 + (1|Block), AvgDailyGain))


AdgS1 <- mcmcsamp(fm1Adg, 50000)
library(coda)
HPDinterval(AdgS1)

mcmcpvalue(as.matrix(AdgS1)[, 6:8])

#If we drop the interaction term and consider the treatment term the test becomes

(fm2Adg <- lmer(adg ~ InitWt + Treatment + (1|Block), AvgDailyGain))

AdgS2 <- mcmcsamp(fm2Adg, 50000)
HPDinterval(AdgS2)
mcmcpvalue(as.matrix(AdgS2[,3:5]))

#so these p-values seem to be in line with the results from the
#analysis of variance p-values using one of the formula for calculation
#of a residual degrees of freedom.

#Related to the other question of the use of a likelihood ratio test
#for the fixed effects, the p-values for the likelihood ratio tests
#seem quite different

anova(fm2Adg, fm1Adg)

fm3Adg <- lmer(adg ~ InitWt + (1|Block), AvgDailyGain)
anova(fm2Adg, fm3Adg)

#and it is not just a matter of the evaluation of the log-likelihood

fm2AdgML <- update(fm2Adg, method = "ML")
fm3AdgML <- update(fm3Adg, method = "ML")
anova(fm3AdgML, fm2AdgML)
