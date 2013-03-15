library(smatr)

library(phytools)
nTaxa <- 50
set.seed(6)
x <- rnorm(nTaxa)
y <- 2 * x + rnorm(nTaxa, sd = 0.1)
names(x) <- tree$tip.label
names(y) <- tree$tip.label

tree <- rtree(nTaxa)

fm <- lm(y ~ x)
coef(fm)[2] / sqrt(summary(fm)$r.squared)

sma(y ~ x, slope.test = 2)

phyl.RMA(x, y, tree = tree, method = "BM")

phyl.RMA(x, y, tree = tree, method = "BM")$RMA.beta
phyl.RMA(x, y, tree = compute.brlen(tree, method = "rho", 0.5), method = "BM")$RMA.beta
phyl.RMA(x, y, tree = compute.brlen(tree, method = "rho", 0.1), method = "BM")$RMA.beta
phyl.RMA(x, y, tree = compute.brlen(tree, method = "rho", 0.001), method = "BM")$RMA.beta
phyl.RMA(x, y, tree = compute.brlen(tree, method = "rho", 0.0001), method = "BM")$RMA.beta
phyl.RMA(x, y, tree = compute.brlen(tree, method = "rho", 0.00001), method = "BM")$RMA.beta

se.rma <- function(rma, x) {
  n <- length(rma$resid)
  MSresid <- sqrt(sum(rma$resid^2)) / (n-2)
  se <-  sqrt(MSresid / sum((x - mean(x))^2))
  se
}

slopetest.rma <- function(rma, x, y, b0 = 0) {
  se <- se.ols(x, y)$se.b
  b <- rma$RMA.beta[2]
  t <- (b - b0) / se
  df <- length(rma$resid) - 2
  p <- 2 * pt(q = abs(t), df = df, lower.tail = FALSE)
  out <- list(b = b, b0 = b0, t = t, df = df, p = p)
  return(out)
}

se.ols <- function(x, y) {
  fm <- lm(y ~ x)
  summary.fm <- summary(fm)
  se.b <- summary.fm$coefficients[2, 2]
  se.a <- summary.fm$coefficients[1, 2]
  return(list(se.b = se.b, se.a = se.a))
}

rma <- phyl.RMA(x, y, 
                tree = compute.brlen(tree, method = "rho", 0.0000001), 
                method = "BM")
rma
se.ols(x, y)

slopetest.rma(rma, x, y, b0 = 2)


