df <- data.frame(x = runif(10), y = runif(10), sex =
sample(c("m","f"), 10, rep = T))
qplot(x, y, data = df, shape = sex, size = I(5)) +
 scale_shape_manual(values = c("m" = "\u2642", f = "\u2640"))
