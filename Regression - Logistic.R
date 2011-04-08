# Donner Party Example from Statistical Sleuth p. 571

donner <- read.table('/Users/kmm/Documents/Work/R/Statistical Sleuth/ascii/CASE2001.ASC', header = TRUE)

donner$male <- factor(donner$male, levels = c(1, 0))

str(donner)

summary(glm(survival ~ age + male, data = donner, family = 'binomial'))


