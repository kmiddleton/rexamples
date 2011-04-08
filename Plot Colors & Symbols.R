mypch <- c(a=19, b=19, c=19, d=22) #point type
mycol <- c(a='green', b='red', c='black', d='blue') #color
mydf <- data.frame(x=c('a','b', 'b','c','d'), y=c(2, 4, 8, 6, 2))
plot(mydf$y, type='p', pch=mypch[mydf$x], col=mycol[mydf$x])