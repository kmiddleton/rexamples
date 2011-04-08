x <- rnorm(100)
y <- 3 + (2 * x) + rnorm(100, mean = 0, sd = 0.5)
plot(x, y)
mod <- lm(y ~ x)
mod
summary(mod)

mod.resi <- resid(mod)
qqnorm(mod.resi)
qqline(mod.resi)

# or
shapiro.test(mod$res)
plot(mod, 2)
text(par("usr")[1]+1, par("usr")[4]-.5, labels=paste("p-value",
	round(shapiro.test(mod$res)$p.value, 4), sep="="))

# Use plot(mod) to cycle through the diagnostic plots for lm class objects

# shapiro.test() tests for normality (use this on raw data and/or residuals)