library(MCMCglmm)

set.seed(1)

data(bird.families) 

phylo.effect <- rbv(bird.families, 1, nodes="TIPS") 
phenotype <- phylo.effect + rnorm(dim(phylo.effect)[1], 0, 1)  

# simulate phylogenetic and residual effects with unit variance

test.data <- data.frame(phenotype=phenotype, animal=row.names(phenotype))

prior <- list(R = list(V = 1, nu = 1), G = list(G1 = list(V = 1, nu = 1)))

model2<-MCMCglmm(phenotype ~ 1, random = ~animal, data = test.data, 
  pedigree = bird.families, prior = prior, verbose = FALSE,
  nitt=130000, thin=100, burnin=30000)

plot(model2$VCV)
diag(autocorr(model2$VCV)[2, , ])
effectiveSize(model2$VCV)

cor(model2$VCV)
HPDinterval(model2$VCV)
summary(model2$Sol)

