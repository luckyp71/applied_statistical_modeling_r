# ----------------------------- Statistical Inference
require(dplyr)
require(ggplot2)
require(plotly)
require(datasets)
require(MASS)

data <- ToothGrowth

v <- data%>%
  ggplot(aes(x=supp, y=len))+
  geom_point(aes(color=I('blue')), size=1)+
  geom_boxplot(aes(fill=supp))+
  theme_minimal()

ggplotly(v, tooltip = c('x', 'y'))

# shapiro-wilk normality test
#H0: data are normally distributed
shapiro.test(data$len) # the p-value is greater than 0.05, so accept the null hypothesis, that the data is normally distributed

# one sided t-test
# test if the mean value is equal to a certain number
# H0: true value of mean=18
mean(data$len)
t.test(data$len, mu=18) # accept the null hypo

t.test(data$len, alternative = 'greater', mu=5) # reject the null hypo, accept the alternative hypo

# independent 2-group t-test
# test the difference in mean
# H0: There is no differenc in the population means of the 2 groups
oj <- data%>%
  filter(supp=='OJ')%>%
  pull(len)

vc <- data%>%
  filter(supp=='VC')%>%
  pull(len)

t.test(oj, vc, 
       paired = F,
       var.equal = F,
       conf.level = 0.95) # accept the null hypo


t.test(oj, vc,
       alternative = 'greater', paired = F) # accept the alternative hypo

# paired observations: if we collect two measurements on each item
t.test(oj, vc,
       mu=2,
       paired = T,
       var.equal = F,
       conf.level = 0.95) # p-value is greater than 0.05, accept the null hypo
