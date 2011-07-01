library(foreach)

foreach(i = 1:3) %do% sqrt(i)

foreach(a = 1:3, b = rep(10, 3)) %do% {a + b}

foreach(i = 1:3, .combine = "c") %do% exp(i)

foreach(i = 1:4, .combine = "cbind") %do% rnorm(4)

foreach(i = 1:4, .combine = "+") %do% rnorm(4)

foreach(a = irnorm(4, count = 1000), .combine = "+") %do% a

foreach(icount(1000), .combine = "+") %do% rnorm(4)

foreach(a = irnorm(1, count = 10), .combine = "c") %:% when(a >= 0) %do% sqrt(a)
