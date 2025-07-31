# ---------------- Logistic Regression
install.packages('ResourceSelection')
install.packages('arm')
install.packages('AICcmodavg')

require(datasets)
require(dplyr)
require(ggplot2)
require(plotly)
require(ResourceSelection)
require(arm)
require(AICcmodavg)

data <- mtcars
head(data)
glimpse(data)

unique(data$am)

data%>%
  ggplot(aes(x=wt, y=am))+
  geom_point()+
  stat_smooth(method='glm', method.args=list(family='binomial'),
              se = F)+
  theme_minimal()+
  theme(panel.grid = element_blank())

fitglm <- glm(am~hp+wt, data=data, 
              family = binomial(link = 'logit'))

exp(coef(fitglm))

summary(fitglm)
# the null deviance shows how will the response variable is predicted
# by a model that includes only the intercept (grand mean)
# DF number of observation - 1

# the residual deviance shows how will the response variable is predicted by 
# a model that includes both predictor vars (DF declines by 2 more)

# residual deviance for a well-fitting model should be approximately
# equal to its degrees of freedom

# ----------- How well does the model fit the data
# Hosmer and Lemeshow goodness of fit (GOF) test
hoslem.test(data$am, fitted(fitglm))
# the p-value is greater than 0.05, accept the H0 that the model appears to fit well; no significant difference between the model and the observed data

# predicting on unseen data
newdata <- data.frame(hp=120, wt=2.8)
predict(fitglm, newdata, type='response')

# Overdispersion means that the data show discrepancies between the observed response yi
# and their predicted value larger than what the binomial model would predict.
# overdispersion is present in a dataset, the estimated standard errors, test statistics
# and overall goodness-of-fit will be distorted
x = predict(fitglm)
y =  resid(fitglm)

binnedplot(x,y) # most o the data fall between -2 and 2 standard error
# no overdispersion


# Binomial count data
# logistic data for other cases with sinosuidal shape
# variance decreases towards 0 and 1
# binomial distribution
data("beetle")
data <- beetle

head(data)
names(data) <- tolower(names(data))
head(data)

data <- data%>%
  mutate(survive = number_tested - number_killed,
         survive_proportion = number_killed/(number_tested - number_killed))

fitglm2 <- glm(cbind(number_killed, survive) ~ dose, data=data, family = binomial)
summary(fitglm2)

# check overdispersion
x <- predict(fitglm2)
y <- resid(fitglm2) 

binnedplot(x,y)


