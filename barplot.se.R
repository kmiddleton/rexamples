##' Barplot with error bars
##'
##' Barplot with error bars, when the error bars (e.g., standard
##' error) have already been calculated.
##' 
##' @title barplot.se
##' @param ys Vector of heights for bars
##' @param upper Vector of upper limit
##' @param lower Vector of lower limit
##' @param lwd Line width (optional)
##' @param xlabs Labels for bars
##' @author Kevin Middleton
barplot.se <- function(ys, upper, lower, lwd = 1, xlabs){
  bp <- as.numeric(barplot(ys))
  ytop <- max(upper)
  barplot(ys, ylim = c(0, ytop), names.arg = xlabs)
  segments(bp, lower, bp, upper, lwd = lwd)
}

ys <- runif(8, 10, 12)
se <- runif(8)
upper <- ys + se
lower <- ys - se
barplot.se(ys, upper, lower, lwd = 2, xlabs = 1:8)
