library(lme4)
library(languageR)

fm2 <- lmer(Yield ~ 1 + (1|Batch), Dyestuff)

summary(fm2)

mcmc <- pvals.fnc(fm2, nsim=10000, withMCMC=TRUE)
mcmc$fixed
mcmc$random

aovlmer.fnc(fm2, mcmc$mcmc, c("Batch (Intercept)"))
