library(lme4)

data(Pastes)
dummy <- rep(1,nrow(Pastes))
m1 <- lmer(strength ~ (1|dummy) + (1|batch), Pastes, REML=F)
m0 <- lmer(strength ~ (1|dummy), Pastes, REML=F)
anova(m1, m0)
