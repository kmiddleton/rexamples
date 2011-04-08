# function
dailyWeather <- function(){
require(RCurl)
require(XML)

# Get data and parse into a list
z <- unlist(xpathApply(htmlTreeParse(getURL('http://weather.csusb.edu/WXToday.htm', useragent = "curl"), useInt = TRUE), '//td', function(x) xmlValue(x)))

num.obs <- length(z)/16

times <- z[seq(1, length(z), by = 16)]
times <- gsub("a", " AM", times)
times <- gsub("p", " PM", times)
times <- strptime(times, format = "%m/%d/%Y %I:%M:%S %p")

dat <- as.numeric(z[-seq(1, length(z), by = 16)])
dat.df <- as.data.frame(matrix(dat, ncol = 15, byrow = TRUE))
names(dat.df) <- c('wind.dir', 'wind.spd.mph', 'wind.gust.mph', 'hum.in.pct', 'hum.pct',
				   'temp.in', 'temp.raw', 'barom.in', 'tot.rain', 'UV', 'solar', 'daily.rain',
				   'hour.rain', '24hr.rain', 'rain.rate')
df.out <- data.frame(times, dat.df)
df.out
}
###############################################################################################


today <- dailyWeather()

par(mfrow = c(2, 1))
plot(today$times, today$temp.raw, type = 'l')
max.time <- today$times[today$temp.raw == max(today$temp.raw)]
max.time <- max.time[length(max.time)]
max.temp <- max(today$temp.raw)
points(max.time, max.temp, col = 'red', pch = 16, cex = 1.5)

plot(today$times, today$hum.pct, type = 'l')

#'http://weather.csusb.edu/Summary.htm'

