# ------------ Poisson Regression
# for count data
# 1) being discrete and 2) having variance that generally increases with the mean

require(dplyr)
require(ggplot2)
require(plotly)
require(MASS)


data <- read.csv('./data/canopycvr1.csv')
head(data)

sort(unique(data$cover))

mean(data$cover)
var(data$cover)
# our data mean and variance to be roughly similar,
# because they roughly similar, they'll have the same pattern of increasing and that's
# usually the case with count data

# predict variation in canopy cover as a function of elevation
fit <- glm(cover~elev, data=data, family=poisson(link=log))
summary(fit)

coeffs <- coef(fit)
coeffs

# for a unit increase in elevation the increase in cover is e^b

exp(coeffs[2]) # with every unit increase in elevation
# cover increases by a factor of 1.002
# inverse of the link function


(exp(coeffs[2])-1)*100

# model 2: add predictor variable
fit2 <- glm(cover ~ elev+tci, data=data, family = poisson)
summary(fit2) # elevation is statistically significant while tci is not

# compare models
anova(fit, fit2, test='Chisq') #p-value > 0.05, tci has not improved the model performance

# In case of overdisprsd data, use negative binomial regression
fit3 <- glm.nb(cover~elev, data=data)
summary(fit3)
