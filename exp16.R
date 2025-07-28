# --------------------- ANCOVA: Analysis of Co-Variance
# ANCOVA is used to compare two or more regression lines by testing
# the effect of a categorical factor on a dependent variable (y-var)
# while controlling for the effect of a continuous co-variable (x-var)

# condition: 1) linear scatter 2) normal residuals

require(smatr)
require(datasets)
require(dplyr)
require(ggplot)
require(plotly)

data(leaflife, package = "smatr")
data <- leaflife
glimpse(data)
summary(data)

plot(data$lma, data$longev)

fit1 <- lm(longev ~ lma*rain, data=data)
summary(fit1)
# the model can explain 48,9% variation in longev, indicating a moderately good fit.
# the intercept: p-value is greater than 0.05 (accept the H0), meaning it is not statistically significant; when lma=0 and rain=high, the longev is not significantly different from 0
# lma: p-value is less than 0.05 (reject the H0), meaning it is statistically significant;there is an effect/contribution of lma on longev when rain=high (baseline condition)
# rainlow: p-value is greater 0.05 (accept the H0), meaning it is not statistically significant; there is no effect/contribution of rain=low on longev when lma=0
# lma:rainlow (interaction term): p-value is greater than 0.05 (accept the H0), meaning it is not statistically significant; there is no significant interaction, i.e. the effect of lma on longev does not significantly differ under rain=low


fit2 <- lm(longev ~ lma+rain, data=data)
summary(fit2)

# test whether include the interaction term (lma:rain) significantly improves the model?
anova(fit1, fit2)
# fit1: includes main effects + interaction 
# ffit2: only main effects
# the p-value is 0.78, so the interaction term is not statistically significant
# accept the H0 that the interaction has no effect, so model 1 does not significantly improve upong model 2

# regression for each rain category

high_r <- data%>%
  filter(rain=='high')

low_r <- data%>%
  filter(rain=='low')


# regression for high rain
reg1 <- lm(longev~lma, data=high_r)
summary(reg1)
confint(reg1)

# regression for low rain
reg2 <- lm(longev~lma, data=low_r)
summary(reg2)
confint(reg2)

# H0: slopes don't overlap
# if the confidence intervals of slop overlap, the slopes overlap
