getColorName <- function(colorNumber) colors()[colorNumber]
printColorSampler <- function(n = 0) {
   i <- seq(colors())
   k <- ceiling(sqrt(length(i)))
   xy <- cbind(floor(i/k)*2, i %% k)
   plot(xy, type = "n", axes = FALSE, xlab = "", ylab = "")
   text(xy[,1]+.5, xy[,2]+.2, i, col = colors(), cex = 1)
   if (n > 0)
      colors()[identify(xy, n = n, labels = colors(), plot = FALSE)]
}
# test
printColorSampler(1)