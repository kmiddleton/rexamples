library(lme4)
(model1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))

vc       <- VarCorr( model1 )
varcomps <- c(unlist( lapply(vc, diag) ), # random intercept variances
            attr(vc,"sc")^2)              # residual variance


library(statmod)
library(MASS)
data(petrol)
out <- mixedModel2(Y~SG+VP+V10+EP, random=No, data=petrol)
cbind(varcomp=out$varcomp,se=out$se.varcomp)


library(nlme)
Petrol <- petrol
Petrol[, 2:5] <- scale(as.matrix(Petrol[, 2:5]), scale = FALSE)
pet3.lme <- lme(Y ~ SG + VP + V10 + EP,
                random = ~ 1 | No, data = Petrol)
pet3.lme <- update(pet3.lme, method = "ML")
pet4.lme <- update(pet3.lme, fixed = Y ~ V10 + EP)
anova(pet4.lme, pet3.lme)