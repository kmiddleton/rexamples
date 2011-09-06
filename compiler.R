myfun <- function(n){
  x <- runif(n)
  out <- numeric(length = n)
  for (i in seq_len(n)){
    out[i] <- ifelse(x[i] < 0.5, TRUE, FALSE)
  }
  return(out)
}

library(compiler)
cfun <- cmpfun(myfun)

n <- 1000
reps <- 10000

library(benchmark)
benchmark(myfun(n), cfun(n),
          columns = c("test", "replications", "elapsed", "relative"),
          order = "relative", replications = reps)
