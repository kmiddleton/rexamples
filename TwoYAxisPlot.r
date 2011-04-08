# plots an x y1 y2 using left and right axes for the different y's
# rescales y2 to fit in the same space as y1
TwoVarPlot <- function(xvar, y1var, y2var, labels, noincs = 5,
marks=c(1,2), legpos, leglabs, title)
{
# rescale to fit on same axis
scaledy2var <- (y2var - min(y2var)) / (max(y2var) - min(y2var))
scaledy2var <- (scaledy2var * (max(y1var) - min(y1var))) + min(y1var)

# plot it up and add the points
plot(xvar, y1var, xlab=labels[1], ylab="", axes=F, pch=marks[1],
main=title)
points(xvar, scaledy2var, pch=marks[2])

# make up some labels and positions
y1labs <- round(seq(min(y1var), max(y1var), length=noincs),2)

# convert these to the y2 axis scaling
y2labs <- (y1labs - min(y1var)) / (max(y1var) - min(y1var))
y2labs <- (y2labs * (max(y2var) - min(y2var))) + min(y2var)
y2labs <- round(y2labs, 2)

axis(1)
axis(2, at=y1labs, labels=y1labs)
axis(4, at=y1labs, labels=y2labs)
mtext(labels[3], side=4, line=2)
mtext(labels[2], side=2, line=2)
box()

legend(legpos[1], legpos[2], legend=leglabs, pch=marks, bty="n")
} 