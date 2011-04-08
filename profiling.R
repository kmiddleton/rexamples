library(MASS); library(boot) 
storm.fm <- nls(Time ~ b*Viscosity/(Wt - c), stormer, start = c(b=29.401, c=2.2183)) 
st <- cbind(stormer, fit=fitted(storm.fm)) 
storm.bf <- function(rs, i) { 
st$Time <- st$fit + rs[i] 
tmp <- nls(Time ~ (b * Viscosity)/(Wt - c), st, start = coef(storm.fm)) 
tmp$m$getAllPars() 
} 
# remove mean 
rs <- scale(resid(storm.fm), scale = FALSE) 
Rprof("boot.out") 
# pretty slow 
storm.boot <- boot(rs, storm.bf, R = 4999) 
Rprof(NULL) 

library(profr)
library(ggplot2)
pr <- profr(storm.boot <- boot(rs, storm.bf, R = 199)) 
plot(pr) 
ggplot(pr)

sillysum <- function(N) {s <- 0; 
for (i in 1:N) s <- s + i; s} 
ival <- 1/5000 
Rprof("sillysum.out", interval=ival) 
a <- sillysum(1e6); Rprof(NULL) 
plot(parse_rprof("sillysum.out", interval=ival))


efficientsum <- function(N) { s <- sum(seq(1,N)); s } 
ival <- 1/5000 
Rprof("effsum.out", interval=ival) 
a <- efficientsum(1e7); Rprof(NULL) 
plot(parse_rprof("effsum.out", interval=ival)) 


library(jit) 
sillysum <- function(N) { jit(1); s <- 0; for (i in 1:N) s <- s + i; return(s) } 
system.time(print(sillysum(1e7)))
