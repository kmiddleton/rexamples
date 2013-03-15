library(ggplot2)
library(plyr)
library(grid)

rw <- function(n?postgen){
  xy <- matrix(NA, nrow = ngen, ncol = 2)
  xy[1, ] <- c(0, 0)
  for (i in 2:ngen){
    move <- sample(1:4, 1)
    if(move == 1) xy[i, ] <- c(xy[i - 1, 1] + 1, xy[i - 1, 2])
    if(move == 2) xy[i, ] <- c(xy[i - 1, 1] - 1, xy[i - 1, 2])
    if(move == 3) xy[i, ] <- c(xy[i - 1, 1], xy[i - 1, 2] + 1)
    if(move == 4) xy[i, ] <- c(xy[i - 1, 1], xy[i - 1, 2] - 1)
  }
  colnames(xy) <- c("x", "y")
  as.data.frame(xy)
}

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
###
walk <- rw(10000)
ggplot(walk, aes(x = x, y = y)) + geom_path(lwd = 2) +
  geom_point(aes(x = 0, y = 0), color = "green", cex = 5) + 
  geom_point(aes(x = walk[nrow(walk), 1], y = walk[nrow(walk), 2]), 
             color = "red", cex = 5)

###
reps <- 1000000
walklength <- c(10, 50, 250)
counter <- 1
endpts <- matrix(NA, nrow = reps * length(walklength), ncol = 3)
for (j in walklength){
  print(j)
  for (i in 1:reps){
    endpts[counter, 1] <- j
    endpts[counter, 2:3] <- as.numeric(rw(j)[j, ])
    counter <- counter + 1
  }
}

endpts <- as.data.frame(endpts)
colnames(endpts) <- c("Walk", "x", "y")
endpts$Walk <- factor(endpts$Walk)

###
distance <- function(x, y) sqrt(x^2 + y^2)
dists <- ddply(endpts, .(Walk), summarise, r = distance(x, y))
mean_dists <- aggregate(. ~ Walk, dists, FUN = mean)

c10 <- cbind(Walk = rep(10, times = 100), 
             circleFun(diameter = mean_dists[1, 2]))
c50 <- cbind(Walk = rep(50, times = 100), 
             circleFun(diameter = mean_dists[2, 2]))
c250 <- cbind(Walk = rep(250, times = 100), 
             circleFun(diameter = mean_dists[3, 2]))
cir <- rbind(c10, c50, c250)
cir$Walk <- factor(cir$Walk)

ggplot(endpts, aes(x = x, y = y)) + 
  geom_hex() +
#  geom_jitter(alpha = 0.1, cex = 1) +
#  geom_point(aes(x = 0, y = 0), color = "red", cex = 5) +
  geom_point(data = cir, aes(x = x, y = y, color = Walk)) +
  coord_equal(ratio = 1) + facet_grid(. ~ Walk)
mean_dists
sqrt(walklength)
