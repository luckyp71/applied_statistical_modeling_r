# ----------------- Linear Regression
require(dplyr)
require(ggplot2)
require(plotly)

data <- iris
names(iris)
head(iris)

# simple linear regression
fit1 <- lm(petal_length ~ sepal_length, data=data)
summary(fit1) # the p-value is much lower than 0.05, so we can say that this is essentially statistically significant, and this model explain 75.83% variation in petal length
# the intercept: -7.10 and slope 1.85843

# multiple regression
fit2 <- lm(petal_length ~ ., data=data)

fit2 <- lm(petal_length ~ .-species, data=data) # exclude species
summary(fit2)
