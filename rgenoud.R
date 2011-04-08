library(rgenoud)

myfunc <- function(x) {
  x1 <- x[1]
   x2 <- x[2]
   abs(x1-x2)
 }

optim(c(0.5,0.5),myfunc,lower=c(0,0),upper=c(1,1),method="L-BFGS-B",control=list(fnscale=-1))

genoud(myfunc, nvars=2, Domains=rbind(c(0,1),c(0,1)),max=TRUE,boundary.enforcement=2)

myfunc <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  (x1-x2)^2
}

optim(c(0.2,0.2),myfunc,lower=c(0,0),upper=c(1,1),method="L-BFGS-B",control=list(fnscale=-1))
genoud(myfunc, nvars=2, Domains=rbind(c(0,1),c(0,1)),max=TRUE,boundary.enforcement=2)