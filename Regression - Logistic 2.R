# Tabachnik Table 12.1
# Example of logistic regression

fall <- c(1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0)
difficulty <- c(3, 1, 1, 2, 3, 2, 1, 3, 2, 2, 2, 2, 3, 2, 3)
season <- factor(c(1, 1, 3, 3, 2, 2, 2, 1, 3, 1, 2, 3, 2, 2, 1), levels = c(3, 1, 2))

fit <- glm(fall ~ difficulty + season, family = 'binomial')

summary(fit)