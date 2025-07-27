require(dplyr)
require(ISwR)
require(datasets)
require(ggplot2)
require(plotly)
require(stats)
require(HH)

data <- rmr
head(data)
names(data) <- gsub('\\.', '_', names(data))

x <- data$body_weight
y <- data$metabolic_rate

fit <- lm(y~x)
summary(fit) # there is statistically significant since the p-value is less than 0.05, there is quantitative dependency between body weight and metabolic rate
abline(fit)  # fit regression line via pts

# plot 95% CI
data%>%
  ggplot(aes(x=body_weight, y=metabolic_rate))+
  geom_point()+
  geom_smooth(method = 'lm')+
  theme_minimal()+
  theme(panel.grid = element_blank())


# plot 99% CI
data%>%
  ggplot(aes(x=body_weight, y=metabolic_rate))+
  geom_point()+
  geom_smooth(method = 'lm', level=0.99)+
  theme_minimal()+
  theme(panel.grid = element_blank())

confint(fit, 'x', level=0.95) #95% CI for slope y=mx+b -> m

ci.plot(fit, xlab='Body Wt', ylab='Metabolic')

# with unseen data, predit CI for a new pt x
newconf <- predict(fit, newdata=data.frame(x=103), interval = "confidence",
        level=0.95)

newconf

# CI for regression line with new data
newx <- seq(120,200, by=20)
fit <- lm(y ~ x)
summary(fit)
plot(x, y, xlab='Body wt', ylab='Metabolic')
abline(fit) # fit regression line via pts
# 95% CI
conf_interval <- predict(fit, newdata=data.frame(x=newx), interval='confidence',
        level=0.95)

conf_interval
lines(newx, conf_interval[, 2], col='blue', lty=2) # lower bound 95% CI
lines(newx, conf_interval[, 3], col='blue', lty=2) # upper bound 95% CI

# 95% Prediction Interval
pred_interval <- predict(fit, newdata=data.frame(x=newx), 
                         interval='prediction',
                         level=0.95)

pred_interval
lines(newx, pred_interval[, 2], col='red', lty=2) # lower bound 95% CI
lines(newx, pred_interval[, 3], col='red', lty=2) # upper bound 95% CI







