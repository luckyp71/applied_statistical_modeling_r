# Violation of Linear Regression Condition: Transform Variable

install.packages('Metrics')
install.packages('quantreg')

require(datasets)
require(dplyr)
require(ggplot2)
require(plotly)
require(MASS)
require(Metrics)
require(quantreg)

data <- iris
head(iris)

fit1 <- lm(sepal_width ~ petal_width, data=data)
summary(fit1)

par(mfrow=c(2,2))
plot(fit1)

qqnorm(residuals(fit1))
qqline(residuals(fit1))

# chck the response data distribution using histogram
v<- data%>%
  ggplot(aes(x=sepal_width))+
  geom_histogram(aes(fill=I('blue')),bins=11)+
  theme_minimal()+
  theme(panel.grid = element_blank()) # the data is a bit skew to the right

ggplotly(v)

# try a transformation of response variable
data <- data%>%
  mutate(
    # square root
    sepal_width_sq = sqrt(sepal_width),
    # cube root
    sepal_width_cube =  (sepal_width)^(1/3),
    # log
    sepal_width_ln = log(sepal_width)
  )

head(data)

data%>%
  ggplot(aes(x=sepal_width_sq))+
  geom_histogram(bins=11)+
  theme_minimal()

data%>%
  ggplot(aes(x=sepal_width_cube))+
  geom_histogram(bins=11)+
  theme_minimal()

data%>%
  ggplot(aes(x=sepal_width_ln))+
  geom_histogram(bins=11)+
  theme_minimal()


# Linear regression between square root of Y and X
fit2 <- lm(sepal_width_sq~petal_width, data=data)
summary(fit2)
par(mfrow=c(2,2))
plot(fit2)

par(mfrow=c(1,1))
qqnorm(residuals(fit2))
qqline(residuals(fit2))


# transform both x and y
# simplify to log(y)~log(x)
# transform-back y = exp(a + b*log(x))

fit3 <- lm(log(sepal_width)~log(petal_width), data=data)
summary(fit3)

par(mfrow=c(2,2))
plot(fit3)

c <- coef(fit3)
a <- c[[1]]
b <- c[[2]]

backtrans <- exp(a + b*log(data$petal_width))
backtrans

# Box-Cox transformation to avoid shifting through transforms
# faimily of transformation designed to reduce non normality of the error (residual)

par(mfrow=c(1,1))
bc <- boxcox(sepal_width ~ petal_width, data=data)
bc

# which lamda that maximize log-likelihood
trans <- bc$x[which.max(bc$y)]
trans

fit4 <- lm(sepal_width^trans~petal_width, data=data)
summary(fit4)

par(mfrow=c(2,2))
plot(fit4)

# When conditions of OLS are not met
data("faithful")
data <- faithful

# Robust & Resistant regression
# use in any situation we would use OLS and have outliers
fit1 <- lm(eruptions~waiting, data=data)
summary(fit1)

par(mfrow=c(2,2))
plot(fit1)

num <- data[,'waiting', drop=F]
head(num)


p1 <- predict(fit1, num) #produce estimates of eruptions from fit1 model
p1 <- as.data.frame(p1)

rmse(data$eruptions, p1$p1)
mape(data$eruptions, p1$p1)

# Robust regression
# Reduce the influence of outliers
# Downweight outliers-reduce their influence on fitted regression line
fit2 <- rlm(eruptions~waiting, data=data, psi = psi.bisquare)
summary(fit2)
p2 <- predict(fit2, num)
head(p2)
p2 <- as.data.frame(p2)
rmse(data$eruptions, p2$p2) # with this data, using robust regression doesnt give us any advantages


# Resistant Regression
# heeavy tailed distribution - outlying points at the end of QQ
# neutralises the effect of outliers
par(mfrow=c(1,1))
qqnorm(residuals(fit1))
qqline(residuals(fit1))

resis <- lqs(eruptions ~ waiting, data=data)

summary(resis)
p2 <- predict(resis, num)
head(p2)
p2 <- as.data.frame(p2)
head(p2)
rmse(data$eruptions, p2$p2)

plot(data$waiting, data$eruptions)

abline(fit1, lty='dashed')



data%>%
  mutate(
    fit1 = predict(fit1),
    robus = predict(fit2),
    resis = predict(resis)
  )%>%
  ggplot(aes(x=waiting, y=eruptions))+
  geom_point()+
  geom_line(aes(y=fit1, color=I('red')), linetype='dashed')+
  geom_line(aes(y=robus, color=I('blue')), linetype='dashed')+
  geom_line(aes(y=resis, color=I('orange')), linetype='dashed')+
  labs(
    x='Waiting',
    y='Eruptions',
    title='Regression Models Comparison'
  )+  
  theme_minimal()+
  theme(panel.grid=element_blank())

# non constant variance or heteroscedasticity
# since our data for waiting and eruptions seem to be clustered as 2-3 clusters, we can check with quantile regression
q25 <- rq(eruptions~waiting, data=data, tau=0.25)
q75 <- rq(eruptions~waiting, data=data, tau=0.75)

anova(q25, q75) #H0: regression coef are same for both

data%>%
  mutate(
    q25 = predict(q25, data.frame(waiting=waiting)),
    q75 = predict(q75, data.frame(waiting=waiting))
  )%>%
  ggplot(aes(x=waiting, y=eruptions))+
  geom_point()+
  geom_line(aes(y=q25, color=I('red')), linetype='dashed')+
  geom_line(aes(y=q75, color=I('blue')), linetype='dashed')+
  labs(
    x='Waiting',
    y='Eruptions',
    title='Quantile Regression'
  )+  
  theme_minimal()+
  theme(panel.grid=element_blank())

