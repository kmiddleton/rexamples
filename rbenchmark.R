library(rbenchmark)

John <- function() all( abs(x - mean(x)) < .Machine$double.eps ^ 0.5 )
DWin <- function() {diff(range(x)) < .Machine$double.eps ^ 0.5}
zero_range <- function() {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = .Machine$double.eps ^ 0.5))
}

x <- runif(500000);

benchmark(John(), DWin(), zero_range(),
          columns=c("test", "replications", "elapsed", "relative"),
          order="relative", replications = 10000)

# Compiler doesn't help with vectorized code
library(compiler)
cJohn <- cmpfun(John)
cDWin <- cmpfun(DWin)
czero_range <- cmpfun(zero_range)
benchmark(cJohn(), cDWin(), czero_range(),
          columns=c("test", "replications", "elapsed", "relative"),
          order="relative", replications = 10000)
