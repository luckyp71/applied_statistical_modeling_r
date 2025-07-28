# Standardize Major Axis (SMA) Regression
# Linear Regression may not be appropriate:
# i) there may be measurement error in x and y (common in observational studies)
# ii) x and y may have different scales/units
# conditions: i) linear relation between x and y ii) normally distributed residuals
require(smatr)
require(datasets)
require(dplyr)
require(ggplot2)
require(plotly)

data <- Orange
head(data)
names(data) <- tolower(names(data))
head(data)

levels(data$tree)
glimpse(data)

# test for common slope between different tree categories
comslope <- sma(circumference~age*as.factor(tree), log="xy", data=data) # consider the interaction between age and tree category
summary(comslope)

# test for common slope between sites and different rain
data <- leaflife
head(data)
levels(data$rain)
comslope <- sma(longev ~ lma * rain, log='xy', data=data)
summary(comslope) 
# R2 values show that 44% (high rain) and 73% (low rain) of the variance in log(longev) is explained by log(lma)
# the relationship is significant (p-values < 0.05), indicating a positive correlation between lma and longev (on log scale)
# no significant difference in slope between rain groups (p-value > 0.05)
# both group models are statistically significant (p-value < 0.05)
# the effect strength (slope) is slightly higher in high rain but not statistically different

plot(comslope)

# Testing for evidence for a given sloe (or scaling factor)
data <- Animals
head(data)

plot(data, log='xy')
fit <- sma(brain ~ body, log='xy', data=data)
summary(fit)

# does brain size scale as the 2/3 power of body size?
# brain=a*body^2/3
# or is slope=1 (variables exhibit equal proportional changes and demonstrate isometry)

fit1 <- sma(brain~body, log='xy', slope.test=1, data=data)
fit2 <- sma(brain~body, log='xy', slope.test=2/3, data=data)

summary(fit1) # rejected the H0, the p-value is less than 0.05, meaning the slope is significantly different from 1
summary(fit2) # the slope is not statistically different from 2/3, accept the H0






