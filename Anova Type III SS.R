# see: http://afni.nimh.nih.gov/sscc/gangc/SS.html

library(car)

A <- as.factor(c(rep(0, 16), rep(1, 16)))

B <- as.factor(c(1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4))

data <- c(3,6,3,4,5,4,3,3,7,8,7,6,7,8,9,8,1,2,2,2,2,3,4,3,5,6,5,6,10,10,9,11)

fit <- aov(data ~ A * B)

summary(fit)

# This matches
Anova(fit, type = "II")

# This doesn't match
Anova(fit, type = 'III')

# This does match.
# Also matches output of SPLUS summary(fit, ssType=3)
# see: http://finzi.psych.upenn.edu/R/Rhelp02a/archive/65554.html
fit2 <- aov(data ~ A * B, contrasts = list(A = contr.helmert, B = contr.helmert))
summary(fit2)

# Also:
# options(contrasts = c("contr.helmert", "contr.poly"))

Anova(fit2, type = "III")

