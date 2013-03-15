library(xlsx)
library("plyr")
library("MASS")

dd <- read.xlsx("Trilobites.xlsx", 1)

colMeans(as.matrix(dd[, 2:4]))

D <- as.matrix(dd[, 2:4])

## Variance/covariance matrix
cov(D)
cor(D)

ZD <- scale(D)

pc <- prcomp(ZD)
pc
biplot(pc)

pcD <- prcomp(D)
biplot(pcD)
