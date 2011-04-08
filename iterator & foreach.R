library(iterators)
library(foreach)

m <- icount(100)
xx <- foreach (i = m) %do% { 2^10 }

# Note: returns a list