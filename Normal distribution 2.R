library(ggplot2)
df <- data.frame(z = seq(-4, 4, length = 100))

base <- qplot(z, dnorm(z), data = df, geom = "area")

base + geom_area(data = subset(df, z < qnorm(0.05)), fill = "red")
base + geom_area(data = subset(df, z > qnorm(0.95)), fill = "red")
base + geom_area(aes(group = sign(z)),
 data = subset(df, abs(z) > qnorm(1 - 0.05 / 2)), fill = "red")