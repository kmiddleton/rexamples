ll <- c(x=10.1, sde=5.5)
plot(1:10)
text(x=9, y=2, pos=2, substitute(X[min] == x %+-% sde, as.list(ll)))
