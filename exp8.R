# ----------------------- Two way ANOVA
# Evaluate the effect o two grouping variables / two factor variables on a response variable
# H0: response mean for all factor levels are equals
require(datasets)
require(dplyr)
require(ggplot2)
require(plotly)
require(car)

data <- ToothGrowth
View(data)

head(data)

data$dose <- factor(data$dose,
        levels=c(0.5,1,2),
       labels = c('D0.5', 'D1', 'D2'))

head(data)

v<- data%>%
  ggplot(aes(x=supp, y=len))+
  geom_boxplot(aes(fill = supp))+
  facet_grid(.~dose)+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        legend.position='none')

ggplotly(v)


# check if tooth length depends on supp and dose.
# two factor variables are independent
md1 <- aov(len ~ supp+dose, data=data)
summary(md1)

# both supp and dose are statistically significant
# mean tooth length for both supp and dose are not equal

# test two variables might interact to create an synergistic effect (interaction effect)
md2 <- aov(len ~ supp*dose, data=data)
summary(md2)

# two main effects (supp and dose) are statistically significant,
# as well as their interaction relationships between dos and tooth
# length depends on the supp method influence the difference between mean
# tooth length

# Post Hoc test to identify which dosage group is different
TukeyHSD(md2, which = "dose") # the mean tooth length different significantly across all dosage levels

# pairwise t-test
pairwise.t.test(data$len, data$dose,
                p.adjust.method = "BH")

# Testing when we have unequal sample numbers
md3 = aov(len ~ supp* dose, data=data)
Anova(md3, type="III")

# Conditions for Anova

# 1. Test homogeneity of variance assumption
plot(md2, 1) # the result is almost horizontal line, so we can conclude that condition of homogeneity of variance is being met

# 2. test normality of residuals
plot(md2, 2)

# extract the residuals
aov_residuals <- residuals(object = md2)
shapiro.test(x=aov_residuals) # p-value is greater than 0.05, accept the null hypo, the errors are normally distributed
