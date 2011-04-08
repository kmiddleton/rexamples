# file: draw.regression.R
# created: 11 Apr 2007

# function:draw.regression 
# 	Draws a regression line only over the range of the data
# 	Pass the x variable (logged if necessary) and an object of type lm
# 	Could write this to handle the logging here.

draw.regression <- function(x, lm.obj, ylog = FALSE, lty = 1, lwd = 2)
{	
	if (ylog == TRUE)
	{
		xmin <- min(log10(x))
		xmax <- max(log10(x))
	
		ymin <- min(predict(lm.obj))
		ymax <- max(predict(lm.obj))

		segments(10^xmin, 10^ymin, 10^xmax, 10^ymax, lty = lty, lwd = lwd)
	} else
	{
		xmin <- min(x)
		xmax <- max(x)
	
		ymin <- min(predict(lm.obj))
		ymax <- max(predict(lm.obj))

		segments(xmin, ymin, xmax, ymax, lty = lty, lwd = lwd)
	}
}


# function: draw.segment
#	Draws a line segment of a given slope and intercept only 
#	over the range of data present in vector x

draw.segment <- function(sl, int, x, xlog = FALSE, lty = 1, lwd = 2){
	if (xlog == TRUE)
	{
		xmin <- min(log10(x))
		xmax <- max(log10(x))
		
		ymin <- sl*xmin + int
		ymax <- sl*xmax + int
		
		segments(10^xmin, 10^ymin, 10^xmax, 10^ymax, lty = lty, lwd = lwd)
	} else
	{
		xmin <- min(x)
		xmax <- max(x)
		
		ymin <- sl*xmin + int
		ymax <- sl*xmax + int
		
		segments(xmin, ymin, xmax, ymax, lty = lty, lwd = lwd)
	}
}