curve(dt(x, 74), from = -4, to = 4)
x <- seq(-1.96, 1.96, len = 100)
y <- dt(x, 74)
polygon(c(x[1], x, x[100]), c(dt(-4, 74), y, dt(4, 74)),
     col = "red", border = NA)