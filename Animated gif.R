set.seed(123)
ydat <- matrix(runif(10000), ncol = 500)
xdat <- matrix(runif(10000), ncol = 500)

jpeg("myplot%03d.jpeg")
for(i in seq_len(nrow(ydat))) 
   plot(xdat[1:i,], ydat[1:i,], ylim = c(0,1), xlim = c(0,1))
dev.off()

# Then in shell
# convert -delay 5 -loop 0 *.jpeg animation.gif