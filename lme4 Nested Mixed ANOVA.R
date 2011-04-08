library(lme4)
data(Oats, package = "MEMSS")

str(Oats)

xyplot(yield ~ nitro | Block, Oats, groups = Variety, aspect = 'xy', auto.key = list(columns = 3, lines = TRUE), type = "o")

print(Om1 <- lmer(yield ~ nitro + (1|Block/Variety), Oats))

ranef(Om1)

