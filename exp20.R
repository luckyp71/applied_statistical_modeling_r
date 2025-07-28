# ------------------ Polynomial and Non Linear Regression
# To fit models that can not be fitted using linear models
# e.g. relationship between x and y is nonn-linear - curvilinear
# lienar in the coefficients b1, b2
# though it may contain terms that are non-linear in the x's

install.packages('nls2')
install.packages('nlshelper')

require(datasets)
require(dplyr)
require(ggplot2)
require(plotly)
require(nls2)
require(nlshelper)

# dummy data to visualize nonlinear relationship
q <- seq(from=0, to=100, by=1)
p <- 0.6
y <- 500 + p*(q-10)^3
plot(q, y, type='l', col='red',
     main='Nonlinear Relationship', lwd=5)

data <- ChickWeight
head(data)
glimpse(data)

levels(data$diet)

cw1 <- data%>%
  filter(diet=='1')

plot(weight~time, data=cw1) # the correlation does not seem to be linear

fit1 <- lm(weight ~ time, data=cw1)
summary(fit1)
AIC(fit1)

# polynomial terms
fit2 <- lm(weight ~time+I(time*time), data=cw1) # I tells R to treat the expression inside it **as is** rather than interpreting it as a special formula operator
summary(fit2) # the model has slightly better ability in explaining the variation in weight
AIC(fit2) # slightly lower AIC compared to fit1

fit3 <- lm(weight~time+I(time*time)+I(time*time*time), data=cw1)
summary(fit3)
AIC(fit3)

# variables inside "I" are correlated and that can be a problem
# product orthogonal polynomials using poly() function
fit2 <- lm(weight~poly(time, 2), data=cw1)
summary(fit2)
AIC(fit2)

fit3 <- lm(weight~poly(time, 3), data=cw1)
summary(fit3)
AIC(fit3)


# Non Linear Regression
# Linear models cannot be used, e.g. in case of growth equations or radioactive decay, because their relationships are inherently non-linear
data <- Loblolly
head(data)
names(data) <- tolower(names(data))
head(data)

v <- data%>%
  ggplot(aes(x=age, y=height))+
  geom_point()+
  geom_line()+
  theme_minimal()+
  theme(panel.grid=element_blank())

ggplotly(v)

x <- data$age
y <- data$height

# nls needs starting values of a, b, and z
# a is asymptote
# b is x
# and z is scaling values
fit4 <- nls2(y~a+b*I(x^z), start=list(a=1, b=1, z=1))
qqnorm(residuals(fit4))
qqline(residuals(fit4))

# Compute R2
rss <- sum(residuals(fit4)^2) # residuals sum of squares
tss <- sum((y-mean(y))^2) # total sum of squares  
r_square <- 1 - (rss/tss)
r_square  
  

# In case when we don't know the starting values
# we can use SSlogis. It helps create the initial estimates of parameters

getInitial(height~SSlogis(age, Asym, xmid, scal), data=data)
# asymc: numeric parameter representing the asymptote (the maximum value of the response variable)
# xmid: x value at inflection
# scal: scale parameter

y_ss <- nls(height~SSlogis(age, Asym, xmid, scal), data=data)
summary(y_ss)

alpha <- coef(y_ss) # extracting coefficients
plot(height~age, data=data, main="Logistic Growth Model of Trees",
     xlab="Age", ylab="Height")

curve(alpha[1]/(1+exp(-(x-alpha[2])/alpha[3])),
      add = T,
      col='blue')


qqnorm(residuals(y_ss))
qqline(residuals(y_ss))

# Gompertz growth model
## Asym*exp(-b2*b3^x)

fit5 <- nls(height~SSgompertz(log(age), Asym, b2, b3),
            data=data)
summary(fit5)
alpha <- coef(fit5)

qqnorm(residuals(fit5))
qqline(residuals(fit5))

# Chapman richard growth model; where tree height growth is modeled as function of time

chapm <- function(x, Asym, b, c)Asym*(1-exp(-b*x))^c
  
# Asym is the maximum value of growth
# c is related to catabolism (destructive metabolism); maximum value 3
# 1 - exp unction helps define actual growth

nls_lob <- nls(height~chapm(age, Asym, b, c), data=data,
    start=list(Asym=100, b=0.1, c=2.5))
alpha <- coef(nls_lob)
alpha







  
  
  
  
  
  
  
  
