library(plotrix)
?triax.plot
data(soils)
triax.plot(soils[1:10,],main="DEFAULT")
triax.plot(soils[1:10,],main="PERCENTAGES (Counterclockwise axes)",
	tick.labels = list(l=seq(10,90,by=10),r=seq(10,90,by=10),
	b=seq(10,90,by=10)),
	pch=3,cc.axes=TRUE)
triax.return<-triax.plot(soils[1:6,],main="GRID AND LEGEND",
	show.grid=TRUE,show.legend=TRUE,col.symbols=1:6,pch=4)
