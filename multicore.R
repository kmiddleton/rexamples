
library(multicore)

myfun <- function(n, weight, group){
  Fs <- numeric(n)
  for (i in 1:n){
    random.group <- sample(group)
    random.D9 <- lm(weight ~ random.group)
    Fs[i] <- as.numeric(summary(random.D9)$fstatistic[1])
  }
  return(Fs)
}

## Some data
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels=c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
F.obs <- as.numeric(summary(lm.D9)$fstatistic[1])
F.obs

ntot <- 10000
cores <- 2
n <- ntot / cores
procs <- paste("c", 1:cores, sep = "")
for (i in 1:cores){
  procname <- procs[i]
  assign(procname, parallel(myfun(n, weight, group), 
                            mc.set.seed = TRUE))
}
Fs.list <- collect(wait = TRUE)

## Recombine
F.null <- as.numeric(unlist(Fs.list))
F.null[1] <- F.obs
mean(F.null >= F.obs)
summary(lm.D9)