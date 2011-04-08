library(candisc)

grass.mod <- lm(cbind(N1,N9,N27,N81,N243) ~ Block + Species, data=Grass)
Anova(grass.mod,test="Wilks")

grass.can1 <-candisc(grass.mod, term="Species")
plot(grass.can1, type="n")

# library(heplots)
heplot(grass.can1, scale=6)

grass.can1