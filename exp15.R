# -------------- regression with categorical and interaction effect
require(datasets)
require(dplyr)
require(ggplot2)
require(plotly)

# correlation check
qplot(sepal_length, petal_length, data=iris)

fitlm <- lm(petal_length~sepal_length, data=iris)
summary(fitlm)

# check if species have role in influencing the variation
qplot(sepal_length, petal_length, data = iris, color=species)


# is species a significant factor?
x <- lm(petal_length~sepal_length+species, data=iris) # + sign will make no interaction between sepal length and species
summary(x)

# is there any significant variation in petal length across species
fit <- lm(petal_length~sepal_length*species, data=iris)
summary(fit)
anova(fit)

summary(iris$species)


fit2 <- lm(sepal_length~petal_length*petal_width, data=iris)
summary(fit2)
