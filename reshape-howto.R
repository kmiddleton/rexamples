df <- data.frame(LAB = rep(1:8, each=60), BATCH = rep(c(1,2), 240), Y=rnorm(480))

dfm <- melt(df, measure.var="Y")

(thanks to Chuck for pointing that out)


cast(dfm, LAB  ~ ., c(mean, sd, length))
cast(dfm, LAB + BATCH ~ ., c(mean, sd, length))
cast(dfm, LAB + BATCH ~ ., c(mean, sd, length), margins=T)