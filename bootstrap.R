eds <- function(x, rows) {
   x <- x[rows, , drop = FALSE]
   r <- cor(x)
   paren <- 1 - abs(r)/2
   denom <- sum(sum(paren) + 0.5)
   desvio <- sd(x)
   media <- mean(x)
   a <- desvio/media
   nom <- sum(a*(sum(paren) + 0.5))
   eds_abs <- nom/denom
   eds_abs
}

x <- data.frame(var1 = c(23, 657, 67, 89, 23, 657, 67, 89),
                 var2 = c(23, 45, 67, 12, 23, 657, 67, 89),
                 var3 = c(25, 2009, 89, 223, 23, 45, 67, 12),
                 var4 = c(1299, 456, 789, 2,  23, 45, 67, 12))

library(boot)
boot_eds <- boot(x, eds, R = 1000)
plot(boot_eds)
boot_eds