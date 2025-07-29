# -------------------- Linear Mixed Effect Model

# -------------------- Mixed Effect Models - Account for Random Effects
# vary across individuals
# Effects are fixed if they are interesting in themselves
# or random if there is interest in underlying population
install.packages('lme4')
install.packages('lsmeans')
install.packages('lmerTest')

require(dplyr)
require(ggplot2)
require(plotly)
require(datasets)
require(lme4)
require(lsmeans)
require(lmerTest)

# speciffy random effect: (1 } grouping factor)
# random effect model generated for each level of grouping factor
# provide another way to quantify individual differences.

# experiment on the effect of diet on early growth of chicks
# allowing for individual variability in weight of each chick (random)
# (in technical terms, a random intercept for each chick: (1 | chick))

data <- ChickWeight
head(data)
levels(data$chick)
levels(data$diet)
glimpse(data)


# fixed effects of diet & time on the intercept
# a constant difference in weights among chicks
# randomly assigned to different diets
# random intercept model
model1 <- lmer(log(weight) ~ time+diet + (1|chick), data=data,
     REML = T)
summary(model1)


# impact of interaction between diet and time
# quantify the impact on the slope (i.e. effects o diet on the rate of growth)
model2 <- lmer(log(weight)~time*diet + (1|chick), data=data,
              REML = T)
summary(model2)
coeffs <- coef(summary(model2))
p <- pnorm(abs(coeffs[,'t value']), lower.tail = F)*2
# check whether the interaction between time and diet significant
anova(model1, model2) # model2 has much lower AIC and the p-value is less than 0.05, indicating the interaction between time and diet is significant
# all four diets influence weight gain differently

# model 1
data$predicted <- exp(predict(model1))

ggplot(data, aes(time, weight, color=diet))+
  stat_summary(fun.data=mean_se, geom="pointrange")+
  stat_summary(aes(y=predicted), fun=mean, geom='line')+ #diets influence weight gain
  theme_minimal()+
  theme(panel.grid = element_blank())


# model 2
data$predicted <- exp(predict(model2))

ggplot(data, aes(time, weight, color=diet))+
  stat_summary(fun.data=mean_se, geom="pointrange")+
  stat_summary(aes(y=predicted), fun=mean, geom='line')+ #diets influence weight gain
  theme_minimal()+
  theme(panel.grid = element_blank())


coeffs
diet1_slope <- coeffs[,'Estimate'][['time']] # slope for diet 1
# impact of diet 1 on chick weight
exp(diet1_slope)-1 # 6.98% increase

# impact of diet 1 on chicken weight
diet2_slope <- coeffs[, 'Estimate'][['time']]+coeffs[, 'Estimate'][['time:diet2']]
exp(diet2_slope)-1 # 7.86% increase

# Random Slopes (no random intercept): allowing for a different average slope for each diet
model3 <- lmer(log(weight) ~ time*diet + (0+time|chick), 
     data=data)
summary(model3)
# random efffects:
# group chick: the variation between chick in how weight changes over time
# group residual: the variation within chicks

clst <- lstrends(model3, ~diet, var='time')
clst


