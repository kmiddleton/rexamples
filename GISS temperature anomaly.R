#######################################################################################
## R Script to read NASA GISS  monthly global (land & SST) temperatue anomnaly file   #
## Summarize recent trends and generate plot                                          #  
## D Kelly O'Day, http://processtrends.com & http://chartgraphs.wordpress.com         # 
## GISS monthly data import script develodped by http://LearnR.wordpress.com          # 
#######################################################################################
## Setup & Download from web
rm(list=ls()); options(digits=8)
  script <- "GISS_Trend_by_decade.R"
 library(ggplot2)
 par(oma=c(2,1,1,1)) ; par(mar=c(2,4,3,1)); par(xaxs="i"); par(yaxs="i")
 par(ps=9); par(las=1)
################################## Download and process GISS Data File ###############
 url <- c("http://data.giss.nasa.gov/gistemp/tabledata/GLB.Ts+dSST.txt")
 file <- c("GLB.Ts+dSST.txt")
 download.file(url, file)
## Cleanup File
#The first 8 rows and the last 12 rows of the textfile contain instructions 
# Find out the number of rows in a file, and exclude the last 12
  rows <- length(readLines(file)) - 12

# Read file as  char vector, one line per row, Exclude first 7 rows 
  lines <- readLines(file, n=rows)[9:rows]

#Data Manipulation, R vector with 143 lines. 
### Use regexp to replace all the occurences of **** with NA
  lines2 <- gsub("\\*{3,5}", " NA", lines, perl=TRUE)

#Convert the character vector to a dataframe
  df <- read.table(
  textConnection(lines2), header=TRUE, colClasses = "character")
  closeAllConnections()

# We are only interested in the montly data in first 13 columns
  df <- df[,1:13]

# Convert all variables (columns) to numeric format
  df <- colwise(as.numeric) (df)
  #head(df, 12)


# Remove rows where Year=NA from the dataframe
  df <- df [!is.na(df$Year),]
 
# Convert from wide format to long format
  dfm <- melt(df, id.var="Year", variable_name="Month")
  mo_num <- unclass(dfm$Month)
mo_frac <- as.numeric((  unclass(dfm$Month)-0.5)/12)
#mo_frac 
 yr_frac <- dfm$Year + mo_frac
#yr_frac

  temp_anom <- dfm$value/100
  dfm <- data.frame(dfm, mo_num, yr_frac, temp_anom)
  dfm <- dfm[order(dfm$yr_frac), ]
  dfm <- dfm[!is.na(dfm$temp),]
## Find last report month and last value
  last <- nrow(dfm)
  last_yr <- dfm$Year[last]
  last_mo <- dfm$Month[last]
  last_temp <- dfm$temp[last]
 out <- paste("Latest GISS report: " , last_mo, " - ", last_yr, "; Global land & Sea Temp - Anomaly - ", last_temp)
out 
##############################################################################################
## Produce plot and prepare regressions
plot(temp_anom~ yr_frac, data = dfm,  type = "l", col = "darkgrey",
     xlab = "", ylab = "GISS Temperature Anomaly - C",
     xlim = c(1880, 2020), ylim = c(-1,1),  axes = F, pty="m", 
     main = "GISS Temperature Anomaly \nwith Trend Rates By Decade")
axis(1, col="grey")
axis(2, col = "grey")
 grid(col = "lightgrey", lty=1)
# overall trend
flm <- lm(dfm$temp_anom ~ dfm$yr_fr )
   x_min <- min(dfm$yr_frac)
   x_max <- max(dfm$yr_frac)
   y_min <- dfm$temp_anom[1]
   y_max <- dfm$temp_anom[nrow(dfm)]   
   a_flm <- coef(flm)[1] 
   b_flm <- coef(flm)[2]
  x_vals <- c(x_min, x_max)
  y_vals <- c(a_flm +b_flm*x_min, a_flm + b_flm*x_max)
  lines(x_vals, y_vals, col = 139) 
    overall_rate <- signif(b_flm*100,3)
    overall_note_1 <- "Overall Trend - oC/C"
    rect(1882, 0.8, 1920, .96, density = NULL, col = "white", 
    border = "white")
  text(1884, 0.92, adj = 0, overall_note_1, col = 139, cex=0.9)
  text(1892, 0.85, adj=0, overall_rate,col = 139, cex=0.9)

## Calculate monthly regressions
   n <- 200-188
   v_i <- as.numeric(n)
   v_a <- as.numeric(n)
   v_b <- as.numeric(n)
 rect(1910, -0.94, 1980, -0.8, density = NULL, col = "white", 
   border = "white")
 text(1940, -0.89, "Decade Trend Rate -oC per Century", font=3)
for (d in 1:13){
    i = 1870+ d *10
    v_i[d] <- i
    sub <-  subset(dfm, dfm$yr_frac>=i)
    sub <- subset(sub, sub$yr_frac < i+10)
  dlm <- lm(sub$temp_anom ~ sub$yr_frac, data = sub)
  #to set indvidual a & b factors for each decade
     a <- coef(dlm)[1]
     v_a[d] <- a
     b <- coef(dlm)[2]
     v_b[d] <- b
    x_vals <- c(i, i+9.99)
    y_vals <- c(a+b*i, a+b*(i+9.99))

## color code decade trend rate based on <> 0
  if (v_b[d] < 0)  {dec_col = "darkblue"}
  if (v_b[d] >= 0)  {dec_col = "red"}
  lines(x_vals, y_vals, col = dec_col) 
  text(i+1, -0.95, signif(b*100, 2), adj = 0, cex=0.8, col = dec_col)
                     }
## generate dat frame of regression results
 df_regr <- data.frame(v_i, v_a, v_b)
 names(df_regr) <- c("Decade", "a Coef", "b Coef")
 df_regr

## Margin Text
  my_date <- format(Sys.time(), "%m/%d/%y")
  mtext(script, side = 1, line = .5, cex=0.8, outer = T, adj = 0)
  mtext(my_date, side = 1, line =.5, cex = 0.8, outer = T, adj = 1)## Add script name to each plot
  mtext("D Kelly O'Day", side = 1, line =.5, cex = 0.8, outer = T, adj = 0.7)## Add script name to each plot
 data_note <- paste("Land & Sea Stations \n Baseline 1950-1980")

rect(1939, 0.75, 1961, 0.9, density = NULL, col = "white", 
 border = "white")

text(1950, 0.85, data_note, cex = 0.8)
 
