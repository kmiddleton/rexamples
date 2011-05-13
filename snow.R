library(rbenchmark)
library(snow)

cl <- makeCluster(c("localhost","localhost"), type = "SOCK")

tm <- Sys.time()

myfun <- function() {
  x <- runif(500000);
  library(rbenchmark)
  John <- function() all( abs(x - mean(x)) < .Machine$double.eps ^ 0.5 )
  DWin <- function() {diff(range(x)) < .Machine$double.eps ^ 0.5}
  zero_range <- function() {
    if (length(x) == 1) return(TRUE)
    x <- range(x) / mean(x)
    isTRUE(all.equal(x[1], x[2], tolerance = .Machine$double.eps ^ 0.5))
  }
  benchmark(John(), DWin(), zero_range(),
            columns=c("test", "replications", "elapsed", "relative"),
            order="relative", replications = 500)
}

xx <- clusterCall(cl, myfun)
Sys.time() - tm
stopCluster(cl)



tm <- Sys.time()

x <- runif(500000);
library(rbenchmark)
John <- function() all( abs(x - mean(x)) < .Machine$double.eps ^ 0.5 )
DWin <- function() {diff(range(x)) < .Machine$double.eps ^ 0.5}
zero_range <- function() {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = .Machine$double.eps ^ 0.5))
}
benchmark(John(), DWin(), zero_range(),
          columns=c("test", "replications", "elapsed", "relative"),
          order="relative", replications = 1000)
Sys.time() - tm
