library(plyr)

one <- ozone[1, 1, ] 
month <- ordered(rep(1:12, length = 72)) 
model <- rlm(one ~ month - 1) 
deseasf <- resid(model)
library(MASS)
deseasf <- function(value) rlm(value ~ month - 1)

baberuth <- subset(baseball, id == "ruthba01")
baberuth$cyear <- baberuth$year - min(baberuth$year) + 1


calculate_cyear <- function(df) {
	within(df, {
		cyear <- year - min(year) + 1 
		cpercent <- cyear / max(cyear) 
})
}


baseball <- ddply(baseball, .(id), calculate_cyear, .progress = "text")
baseball <- subset(baseball, ab >= 25)

xlim <- range(baseball$cyear, na.rm=TRUE) 
ylim <- range(baseball$rbi / baseball$ab, na.rm=TRUE) 
plotpattern <- function(df) { 
	qplot(cyear, rbi / ab, data = df, geom="line", xlim = xlim, ylim = ylim) 
}

library(ggplot2)
pdf("paths.pdf", width=8, height=4) 
d_ply(baseball, .(reorder(id, rbi / ab)), failwith(NA, plotpattern), .print = TRUE) 
dev.off()