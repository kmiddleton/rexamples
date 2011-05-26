n <- 5
grp <- factor(rep(c("A", "B", "C"), each = n))
y <- rnorm(3 * n)

dd <- data.frame(grp, y)
dd

se <- function (x) {
	n <- length(x)
	sd(x)/sqrt(n)
	}

library(plyr)
ddply(dd, .(grp), function(x) c(xbar = mean(x$y),
                                xse = se(x$y),
                                xmin = min(x$y),
                                xmax = max(x$y)))
                                