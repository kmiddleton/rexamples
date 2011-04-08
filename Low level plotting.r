plot.new()
 plot.window(xlim = c(0, 1),ylim = c(0, 1), asp = 1)
 rect(xleft = .1, ybottom = .1, xright = .9, ytop = .9)
 segments(0, 0, 1, 1)
 segments(0, 1, 1, 0)


 plot.new()
 plot.window(xlim = c(0, 1), ylim = c(0,1), asp = 1)
 p = seq(0, 0.5, length = 21)
 rect(p, p, 1 - p, 1 - p)


 plot.new()
 plot.window(xlim = c(-1.1, 1.1), ylim = c(-1.1,1.1), asp = 1)
 theta = seq(0, 2 * pi, length = 72)
 x = cos(theta)
 y = sin(theta)
 lines(x, y)


 x1 = seq(0, 1, length = 20)
 y1 = rep(0, 20)
 x2 = rep(0, 20)
 y2 = seq(0.75, 0, length = 20)
 plot.new()
 plot.window(xlim = c(0, 1), ylim = c(0,0.75), asp = 1)
 segments(x1, y1, x2, y2)


plot.new()
plot.window(xlim = c(-1, 1), ylim = c(-1,1), asp = 1)
square = seq(0, 2 * pi, length = 5)[1:4]
n = 51
r = rep(1.12, n)
r = cumprod(r)
r = r/r[n]
theta = seq(0, 2 * pi, length = n)
for (i in n:1) {
	x = r[i] * cos(theta[i] + square)
	y = r[i] * sin(theta[i] + square)
	polygon(x, y, col = "gray")
	}


scat = function(x, y) {
	xlim = range(x)
	ylim = range(y)
	plot.new()
	plot.window(xlim = xlim, ylim = ylim)
	points(x, y)
	axis(1)
	axis(2)
	box()
	}
 xv = 1:100
 yv = rnorm(100)
 scat(xv, yv)
 title(main = "My Very Own Scatter Plot")


plot.new()
plot.window(xlim = c(0, 1), ylim = c(5, 10))
abline(a = 6, b = 3)
axis(1)
axis(2)
title(main = "The Overall Title")
title(xlab = "An x-axis label")
title(ylab = "A y-axis label")
box()


# histogram
plot.new()
plot.window(xlim = c(0, 5), ylim = c(0, 10))
rect(0:4, 0, 1:5, c(7, 8, 4, 3), col="lightblue")
axis(1)
axis(2)


plot.new()
plot.window(xlim = c(1, 4), ylim = c(0, 3))
x = c(1, 2, 3, 4)
y = c(0, 2, 1, 3)
lines(x, y)
axis(1)
axis(2)
box()


x = seq(-3, 3, length = 1000)
y = dnorm(x)
plot.new()
plot.window(xlim = range(x), ylim = range(y))
lines(x, y)
axis(1)
axis(2)
box()


plot.new()
plot.window(xlim = c(1920, 1970), xaxs = "i",
ylim = c(46.5, 55.5), yaxs = "i")
abline(v = seq(1930, 1960, by = 10), col = "gray")
abline(h = seq(48, 54, by = 2), col = "gray")
axis(1)
axis(2, las = 1)
box()
title(main = "Average Yearly Temperature")
title(ylab = "Degrees Fahrenheit")
title(xlab = "Year")


pie(rep(1, 36), col = hsv(h=0:35/36))


plot.new()
plot.window(xlim=c(0, 12), ylim=c(0,1), asp=1)
rect(0:11, 0, 1:12, 1, col = hsv(s = 0:11/11))


plot.new()
plot.window(xlim=c(0, 12), ylim=c(0,1), asp=1)
rect(0:11, 0, 1:12, 1, col = hsv(h = seq(1/12, 3/12, length=12)))

 # Do the plot without the x axis
 plot(cars, xlim = c(0, 25), xaxt = "n")

 # Do the x axis in grey with tick marks at 0:25
 # without labels
 axis(1, at = 0:25, labels = NA, col = "grey")

 # Now do the default tick marks in black with labels
 axis(1)