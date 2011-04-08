library(boot)

# use gdata for Mac, xlsReadWrite for Windows
library(gdata)
#library(xlsReadWrite)

# Read the file. Change the path to be the path
dat <- read.xls("/Users/kmm/Desktop/lobke2.xls", header = FALSE)

# Rename the columns
names(dat) <- c("age", "grp", "sex")

set.seed(112)
(median.all <- boot(dat$age, function(x, i){median(x[i])}, 10000))
mean(median.all$t)

(median.ca <- boot(dat$age[dat$grp == "CA"], function(x, i){median(x[i])}, 10000))
mean(median.ca$t)


boot.ci(median.all)

y <- sort(median.all$t)


write.table(median.all$t, file = "outfile.txt", row.names = FALSE, col.names = FALSE)
