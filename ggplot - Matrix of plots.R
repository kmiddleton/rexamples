library(ggplot)

date <- seq(as.Date("2006-11-01"), as.Date("2009-12-01"), by = 1)
date <- rep(date, each=10)

data <- data.frame(
  month = format(date, "%m"),
  year = format(date, "%y"),
  x = rnorm(length(date)),
  y = rnorm(length(date))
)

qplot(x, y, month ~ year, data=data)
