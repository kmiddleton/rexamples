f <- function(a, b){a+b}
x <- c(2, 3)

do.call(f, as.list(x))
