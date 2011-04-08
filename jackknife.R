library(bootstrap)

set.seed(2)
x <- rnorm(20, 10, 2)

mean(x)
var(x)
sd(x)

mean.jk <- jackknife(x, mean)
mean(mean.jk$jack.values)
mean.jk$jack.se

var.jk <- jackknife(x, var)
mean(var.jk$jack.values)
var.jk$jack.se

sd.jk <- jackknife(x, sd)
mean(sd.jk$jack.values)
sd.jk$jack.se
