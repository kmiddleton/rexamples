utils::data(npk, package = "MASS")

## Set orthogonal contrasts.
op <- options(contrasts = c("contr.helmert", "contr.poly"))
( npk.aov <- aov(yield ~ block + N*P*K, npk) )
summary(npk.aov)
F.obs <- summary(npk.aov)[[1]]$"F value"[1]

Fs <- function(data){
  rand.yield <- sample(data$yield)
  rand.aov <- aov(rand.yield ~ block + N*P*K, data)
  F <- summary(rand.aov)[[1]]$"F value"[1]
  return(F)
}


nsims <- 1000
F.null <- c(F.obs, replicate(nsims - 1, Fs(npk)))
mean(F.null >= F.null[1])
