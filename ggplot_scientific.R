library('ggplot2')
library('scales')
DF <- data.frame(x = runif(1000, 0.00000001, 1),
                 y = rnorm(1000))
ggplot(DF, aes(log(x), y)) + geom_point() +
  scale_x_continuous(breaks = (-6):(-1),
                     labels = math_format(10^.x)) +
  xlab('x')
