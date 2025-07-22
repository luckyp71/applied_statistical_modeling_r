# ---------------------------- One-way ANOVA
# This is the extensionn of the independent two samples T test for comparing
# means when we have more than two groups
require(datasets)
require(dplyr)
require(ggplot2)
require(plotly)

data <- PlantGrowth

head(data)
levels(data$group)

v <- data%>%
  ggplot(aes(x=group, y=weight))+
  geom_boxplot(aes(color=group))+
  theme_minimal()+
  theme(legend.position='none',
        panel.grid = element_blank())

ggplotly(v)

# the quantitative variable whose difference we want to check (weight) across three groups and
# the factor variable or the grouping variable
md = aov(weight ~ group, data=data)
summary(md) # the p-value is 0.015 (less than 0.05) and the confidence interval does not include 0, so we can conclude that there are significant differences among the means of the three different groups

# which of the groups are significantly different?
# we can use post hoc test to answer that
t = TukeyHSD(md)
t
plot(t)
# diference between trt2 and trt1 is significant

# Pairwise t-test
pairwise.t.test(data$weight, data$group,
                p.adjust.method = "BH")

# Conditions of one way ANOVA
# 1. Check the homogneity of variance assumption
plot(md, 1)

# 2. Normality of residuals
plot(md, 2)

# Check normality with shapiro-wilk test
aov_residuals <- residuals(object = md) # extract the residual
shapiro.test(x=aov_residuals) # p-value 0.437 (greater than 0.05), we accept the H0 (errors are normally distributed_

# --------------------------- Non-parametric One Way ANOVA

# 1. If the condition of normality of residuals is not met,
# we implement the kruskal wallis test
kruskal.test(weight ~ group, data=data) # the p-value is less tha 0.05, suggestig that oe of the three groups is significantly differet from the other

pairwise.wilcox.test(data$weight, data$group,
                     p.adjust.method = "BH")


# 2. where the homogeneity of variance assumption is violated
oneway.test(weight ~ group, data=data)
