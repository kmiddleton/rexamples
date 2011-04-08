
setwd("/Users/kmm/Documents/Work/R/Neter Data Sets")

dat <- read.table("CH03TA01.txt")

dat2 <- dat[, 2:1]

plot(dat2)

fit <- lm(V1 ~ V2, data = dat2)

summary(fit)

V1.res <- residuals(fit)

plot(V1.res ~ V2, data = dat2)
abline(h = 0)

