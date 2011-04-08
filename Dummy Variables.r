#c is a column, you want dummy variable, one for each valid value. First, make it a factor, then use model.matrix():

> x <- c(2,2,5,3,6,5,NA)
> xf <- factor(x,levels=2:6)
> model.matrix(~xf-1)
  xf2 xf3 xf4 xf5 xf6
1   1   0   0   0   0
2   1   0   0   0   0
3   0   0   0   1   0
4   0   1   0   0   0
5   0   0   0   0   1
6   0   0   0   1   0
attr(,"assign")
[1] 1 1 1 1 1




x <- rnorm(10)
fac1 <- as.factor(c(0,0,0,0,0,1,1,1,1,1))
fac2 <- as.factor(c(1,1,1,2,2,2,3,3,3,4))
y <- data.frame(x, fac1, fac2)
y <- cbind(y, model.matrix(~fac2+fac2-1))