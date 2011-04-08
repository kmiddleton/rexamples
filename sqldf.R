library(sqldf)

sz <- 4e5
bigdf <- data.frame(dim = sample(letters, replace = TRUE, sz), 
	fact1 = rnorm(sz), 
	fact2 = rnorm(sz))
write.csv(bigdf, 'bigdf.csv', quote = FALSE)

f <- file('bigdf.csv')
system.time(bigdf <- sqldf('select * from f', dbname = tempfile(), 
	file.format = list(header = TRUE, row.names = FALSE)))

df2 <- sqldf('select * from f where fact1 > fact2', dbname = tempfile(), 
	file.format = list(header = TRUE, row.names = FALSE))

str(bigdf)
str(df2)

close(f)

#system.time(big.df <- read.csv('bigdf.csv'))

#unlink('bigdf.csv')