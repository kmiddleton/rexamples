library(ggplot)
qplot(wt, mpg, data=mtcars, type=c("point","smooth"), method=lm)