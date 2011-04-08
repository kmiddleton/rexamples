# fit logistic model
my.glm <- glm(vs ~ mpg, data = mtcars, family = binomial(link = "logit"))

# look at a summary
summary(my.glm)

# view the coefficients as odds ratios
exp(coef(my.glm))

install.packages('epicalc')
require(epicalc)
logistic.display(my.glm)
