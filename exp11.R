# ----------------- Linear Regression
require(dplyr)
require(ggplot2)
require(plotly)
require(hier.part)

#devtools::install_github("cran/hier.part")

data <- iris
names(iris)
head(iris)

# simple linear regression
fit1 <- lm(petal_length ~ sepal_length, data=data)
summary(fit1) # the p-value is much lower than 0.05, so we can say that this is essentially statistically significant, and this model explain 75.83% variation in petal length
# the intercept: -7.10 and slope 1.85843

# multiple regression
fit2 <- lm(petal_length ~ .-species, data=data) # exclude species
summary(fit2) # the p-value of intercept is greater than 0.05, meaning it's not statistically significant, so we can exclude it in our linear regression equation
# petal length (y) = 0.73*(sepal_length) - 0.65*(sepal_width) + 1.44*(petal_width)
# each b value is the change in y that a given x can bring on with other xs being constant
# example: while keeping sepal width and petal width constant, a one unit change in sepal_length will will bring about 0.73 in petal length 


# how much the variance in the response variable is explained by eeach predictor or which is the most important predictor in terms of influencing the change in y. 

# variance explained by each predictor
x <- iris[,c(-3,-5)]
head(x)
hier.part(iris$sepal_length, x) # the most important variable in explaining the variation in Y and its contribution to explaining the bespoke variation
