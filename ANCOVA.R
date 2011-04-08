#http://www.ats.ucla.edu/stat/spss/library/hetreg.htm

library(car)
#options("contrasts" = c("contr.treatment", "contr.poly"))

tempdata <- c("  1 1  56 140
 				 2 1  60 155
 				 3 1  64 143
 				 4 1  68 161
				 5 1  72 139
				 6 1  54 159
				 7 1  62 138
				 8 1  65 121
				 9 1  65 161
				10 1  70 145
				11 2  56 117
				12 2  60 125
				13 2  64 133
				14 2  68 141
				15 2  72 149
				16 2  54 109
				17 2  62 128
				18 2  65 131
				19 2  65 131
				20 2  70 145
				21 3  54 211
				22 3  58 223
				23 3  62 235
				24 3  66 247
				25 3  70 259
				26 3  52 201
				27 3  59 228
				28 3  64 245
				29 3  65 241
				30 3  72 269")

temp <- read.table(textConnection(tempdata, open = "r"))[,2:4]

names(temp) <- c("diet", "height", "weight")

plot(temp$height, temp$weight, col=c("red", "green", "blue")[temp$diet])

temp$diet <- factor(temp$diet)

# 1. ANOVA
fit1 <- lm(weight ~ diet, data=temp)
summary(fit1)
anova(fit1)

# 2. Standard ANCOVA
fit2 <- lm(weight ~ diet + height, data=temp)
summary(fit2)
Anova(fit2, type = "III")

# 3. Slopes for each diet group
fit3 <- lm(weight ~ diet * height, data=temp)
summary(fit3)
Anova(fit3, type = "III")
abline(fit3$coefficients[1], fit3$coefficients[4], col = "red")
abline(fit3$coefficients[1]+fit3$coefficients[2], fit3$coefficients[4]+fit3$coefficients[5], col = "green")
abline(fit3$coefficients[1]+fit3$coefficients[3], fit3$coefficients[4]+fit3$coefficients[6], col = "blue")

