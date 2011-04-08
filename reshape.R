smiths <- data.frame(subject = c('john', 'mary'),
				     time = c(1, 1),
				     age = c(33, NA),
				     weight = c(90, NA),
				     height = c(2, 2))

smiths

smithsm <- melt(smiths, id = c("subject", "time"), 
			 measured = c("age", "weight", "height"))

melt(smiths, na.rm = TRUE)

cast(smithsm, time + subject ~ variable) 


trial <- data.frame(id = factor(1:4), A1 = c(1, 2, 1, 2), A2 = c(2, 1, 2, 1), B1 = c(3, 3, 3, 3))
(trialm <- melt(trial))

(trialm <- cbind(trialm, colsplit(trialm$variable, names = c("treatment", "time")))) 

ffm <- melt(french_fries, id = 1:4, na.rm = TRUE)

cast(ffm, . ~ ., length)
mean(ffm$length)

cast(ffm, subject ~ time, length, margins = TRUE)

cast(ffm, variable ~ ., c(min, max))

qplot(value,data=ffm,geom="histogram",facets=.~variable, binwidth=1)

