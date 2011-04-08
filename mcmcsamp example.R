# see http://wiki.r-project.org/rwiki/doku.php?id=guides:lmer-tests

library(lme4)
library(coda)

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

data(AvgDailyGain, package = "SASmixed")
(fm1Adg <- lmer(adg ~ InitWt*Treatment - 1 + (1|Block), AvgDailyGain))

AdgS1 <- mcmcsamp(fm1Adg, 50000)
HPDinterval(AdgS1)

mcmcpvalue(as.matrix(AdgS1)[, 6:8])





mcmcpvalue2 <- function(samp)
{
   std <- backsolve(chol(var(samp)),
                    cbind(0, t(samp)) - mean(samp),
                    transpose = TRUE)
   sqdist <- colSums(std * std)
   sum(sqdist[-1] > sqdist[1])/nrow(samp)
}

(fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
fm1Samp <- mcmcsamp(fm1, 50000)
HPDinterval(fm1Samp)
mcmcpvalue2(as.matrix(fm1Samp)[, 2])

