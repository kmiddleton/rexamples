y <- rep(c(0, 1), each = 20)
x <- factor(rep(c("A", "B", "A", "B"), each = 10))

df <- data.frame(y, x)

fm <- glm(y ~ x, df, family = "binomial")
summary(fm)

K <- diag(length(coef(fm)))
rownames(K) <- levels(fm$model[, 2])             # Assumes that model is fit as Y ~ X
ht <- glht(fm, linfct = K)
print(summary(ht))
plot(ht)
ci <- confint(ht)
ci$confint <- 1 - binomial()$linkinv(ci$confint) # Back-transform
ci$confint[,2:3] <- ci$confint[,3:2]             # Reorders columns so lwr is lower
print(ci)


1 - binomial()$linkinv(0)
