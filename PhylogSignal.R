install.packages("picante",repos="http://R-Forge.R-project.org", dependencies=TRUE)

library(picante)

help(phylosignal)

randtree <- rcoal(20)
randtraits <- evolve.brownian(randtree)

phylosignal(randtraits[randtree$tip.label],randtree)
