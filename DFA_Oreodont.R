library("xlsx")
library("plyr")
library("MASS")

dd <- read.xlsx("Oreodont.xlsx", 1)

ddply(dd, .(Sex), summarize,
      Length = mean(Length),
      Width = mean(Width))

dd.Z <- as.data.frame(scale(dd[1:2]))
dd2 <- cbind(Sex = dd$Sex, dd.Z[, 1:2])

fm <- lda(Sex ~ ., data = dd, method = "moment", CV = TRUE)
fm
plot(fm)

ct <- table(dd2$Sex, fm$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))
