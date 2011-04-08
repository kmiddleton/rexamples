dat <- read.csv('/Users/kmm/Documents/Work/Classes/BIOL 670 - 2008-02 Spring/Tabachnick/table8.9.csv')

dat$case <- factor(dat$case)
dat$trial <- factor(dat$trial)

fit1 <- aov(y ~ x + Error(case * trial), data = dat)
summary(fit1)

library(nlme)
fit2 <- lme(y ~ x, data = dat, random = ~ 1 | case/trial)
summary(fit2)

# Look at last example in car Anova()

library(car)
fit3 <- lm(y ~ x, data = dat)

idata <- data.frame(case = dat$case, trial = dat$trial)

Anova(fit3, idata = idata, idesign = ~ case/trial, type = 'III')

