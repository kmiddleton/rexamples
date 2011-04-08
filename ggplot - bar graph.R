  xx<-"x y g bar
1 1 1 1
1 5 2 3
2 3 1 3
2 4 2 2"
df=read.table(textConnection(xx),header=T);df
  ggplot(df, aes=list(y=y, x=factor(x),bar=bar))->p
  ggbar(p, aes=list(fill=g,barcolour=g+1), avoid="dodge", sort=TRUE)