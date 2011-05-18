
library(ggplot2)
library(gridExtra)

plots <- llply(1:2, function(.x) qplot(1:10,rnorm(10), main=paste("plot",.x))) 

grid.arrange(plots[[1]], plots[[2]], ncol=2, 
             main="test main", sub="subtitle test")
