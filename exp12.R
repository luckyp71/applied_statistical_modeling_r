# ----------------------------------- The Conditions of Linear Regression
install.packages('lmtest')

require(datasets)
require(dplyr)
require(ggplot2)
require(plotly)
require(lmtest)
require(car)

x <- runif(n=100, min=0, max=10)
y <- 1 + 2 * x + rnorm(100, 0, 1)
m <- lm(y ~ x)
par(mfrow=c(2,2))
plot(m)

# multiple linear regression model
fit <- lm(sepal_length ~ petal_length+petal_width, data=iris)
summary(fit) # the model explains 76% of the variation in sepal length
par(mfrow=c(2,2))
plot(fit)


# Test for Autocorrelated/non-independence of Errors
# H0: there is no auto-correlation between errors (errors are independent)
dwtest(fit) # p-value is greater than 0.05, accept the null hypo

# h0: hypo of constant error variance, i.e. no heteroscedasticity
# variance around the regression line is the same for all values of the predictor variable (x)
ncvTest(fit) # p-value is greater than 0.05, accept the null hypo

# identiffy outliers that have too much influence on model
# influential data point
cutoff <- 4/((nrow(iris)-length(fit$coefficients)-2))
cutoff
plot(fit, which=4, cook.levels = cutoff)
