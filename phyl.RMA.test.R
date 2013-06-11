library(smatr)
library(phytools)

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param rma 
##' @param x 
##' @return 
##' @author Kevin Middleton
RMA.SE <- function(rma, x) {
  n <- length(rma$resid)
  MSresid <- sqrt(sum(rma$resid^2)) / (n-2)
  se <-  sqrt(MSresid / sum((x - mean(x))^2))
  return(se)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Phylogenetic RMA Slope Test
##' @param rma 
##' @param x 
##' @param y 
##' @param b0 
##' @return 
##' @author Kevin Middleton
phyl.RMA.slopetest <- function(rma, x, y, b0 = 0) {
  se <- se.ols(x, y)$se.b
  b <- rma$RMA.beta[2]
  t <- (b - b0) / se
  df <- length(rma$resid) - 2
  p <- 2 * pt(q = abs(t), df = df, lower.tail = FALSE)
  out <- list(b = b, b0 = b0, t = t, df = df, p = p)
  return(out)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param x 
##' @param y 
##' @return 
##' @author Kevin Middleton
se.ols <- function(x, y) {
  fm <- lm(y ~ x)
  summary.fm <- summary(fm)
  se.b <- summary.fm$coefficients[2, 2]
  se.a <- summary.fm$coefficients[1, 2]
  return(list(se.b = se.b, se.a = se.a))
}

nTaxa <- 50
set.seed(6)
x <- rnorm(nTaxa)
y <- 2 * x + rnorm(nTaxa, sd = 0.1)
tree <- rtree(nTaxa)
names(x) <- tree$tip.label
names(y) <- tree$tip.label

fm <- lm(y ~ x)

# RMA slope
coef(fm)[2] / sqrt(summary(fm)$r.squared)

sma(y ~ x, slope.test = 2)

phyl.RMA(x, y, tree = tree, method = "BM")

phyl.RMA(x, y, tree = tree, method = "BM")$RMA.beta
phyl.RMA(x, y, tree = compute.brlen(tree, method = "rho", 0.5), 
         method = "BM")$RMA.beta
phyl.RMA(x, y, tree = compute.brlen(tree, method = "rho", 0.1), 
         method = "BM")$RMA.beta
phyl.RMA(x, y, tree = compute.brlen(tree, method = "rho", 0.001), 
         method = "BM")$RMA.beta
phyl.RMA(x, y, tree = compute.brlen(tree, method = "rho", 0.0001), 
         method = "BM")$RMA.beta
phyl.RMA(x, y, tree = compute.brlen(tree, method = "rho", 0.00001), 
         method = "BM")$RMA.beta

rma <- phyl.RMA(x, y, 
                tree = compute.brlen(tree, method = "rho", 0.0000001), 
                method = "BM")
rma
se.ols(x, y)

slopetest.rma(rma, x, y, b0 = 2)


