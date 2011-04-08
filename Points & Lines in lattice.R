library(lattice)

dat<-data.frame(x=1:10,y=1:10,z=sample(letters[1:3],10,T))
xyplot(y~x | z, data = dat,
        panel = function(x, y, ...) {
          panel.lines(x, y, ...)
          panel.points(x, y, col=trellis.par.get("background")$col,
                       cex=1.5, pch=16, ...)
          panel.points(x, y, ...)
        })
