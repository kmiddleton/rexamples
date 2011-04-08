library(forecast)
library(ggplot2)

weightData <- data.frame(weight = c
(2.1,2.4,2.8,3.6,4.1,5.2,6.3),Days=c
(7,14,21,28,35,42,49))
weight <- as.numeric(weightData$weight)
predicted <- forecast(weight,h=3,level=95) 
# see the predicted values by forecast
#predicted
# str(predicted)
forecast <- with(predicted, data.frame(
	weight = as.numeric(mean),
	upper = as.numeric(upper),
	lower = as.numeric(lower),
	Days = seq(max(weightData)+7, by=7, length.out=length(mean)))) ## Change in formula forecast
	
predictValues <- ggplot(weightData, aes(Days, weight)) + geom_line(colour="pink",size=1) + 
	geom_point(size=3,colour="grey45")

predictValues <- predictValues + geom_point(data=forecast,size=5,colour='green') +
	geom_text(data=forecast,label=round(forecast$weight, 1),vjust=2,size=3,colour="blue")

predictValues + scale_x_continuous(breaks=seq(7, by=7, length.out=10))
