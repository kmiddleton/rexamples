require(MASS) ## for oats data set
require(nlme) ## for lme()
require(multcomp) ## for multiple comparison stuff
Aov.mod <- aov(Y ~ N + V + Error(B/V), data = oats)
Lme.mod <- lme(Y ~ N + V, random = ~1 | B/V, data = oats)
summary(Aov.mod)
anova(Lme.mod)
summary(Lme.mod)
summary(glht(Lme.mod, linfct=mcp(V="Tukey")))