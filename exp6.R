# ------------------------ Non-Parametric Alternatives to T-Test
require(dplyr)
require(ggplot2)
require(plotly)
require(datasets)
require(MASS)

co2 <- CO2
View(co2)
names(co2) <- tolower(names(co2))
View(co2)
v <- co2%>%
  ggplot(aes(x=uptake))+
  geom_histogram(aes(fill = I('orange')), binwidth = 5)+
  theme_minimal()

ggplotly(v)

# shapiro-wilk normality test
# H0: data are normally distributed
co2%>%
  pull(uptake)%>%
  shapiro.test() # p-value less than 0.05, reject the null hypo, data are not normally distributed


# Mann-whitney U test: unpaired data
# compare uptake for chilled and non-chilled
# independent samples
chill <- co2 %>%
  filter(treatment=='chilled')%>%
  pull(uptake)

nonchill <- co2 %>%
  filter(treatment=='nonchilled')%>%
  pull(uptake)

wilcox.test(chill, nonchill) # p-value 0.006, reject the null hypo. uptake distributions differ significantly between chilled and nonchilled groups

wilcox.test(co2$uptake~co2$treatment)

View(immer) # Y1 1931 and Y2 1932
wilcox.test(immer$Y1, immer$Y2, paired = T) # reject the null hypo, the yields in both the years are significantly different
