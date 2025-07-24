require(datasets)
require(dplyr)
require(ggplot2)
require(plotly)
require(corrplot)
require(reshape2)

data <- mtcars
View(data)


# check correlation visually with scatter plot
v <- data%>%
  ggplot(aes(x=mpg, y=wt))+
  geom_point()+
  theme_minimal()

ggplotly(v)

data <- iris

head(data)

v <- data%>%
  ggplot(aes(x=petal_length, y=petal_width))+
  geom_point()+
  theme_minimal()

ggplotly(v)

mtcars %>%
  ggplot(aes(x=mpg))+
  geom_histogram(binwidth = 4)

mtcars %>%
  ggplot(aes(x=wt))+
  geom_histogram(binwidth = 4)

# If histogram doesn't give us a clear cut idea about the normality of our distribution
# and we feel that the data, they are still more or less normally distributed then we can use shapiro-wilk test
# H0: data are normally distributed
shapiro.test(mtcars$mpg) # p-value is greater than 0.05, we accept the null hypo that the mpg is normally distributed
shapiro.test(mtcars$wt) # p-value is greater than 0.05, we accept the null hypo that the weight is normally distributed

# since the data meets the parametric testing, we use person's correlation to assess the relationship
cor(mtcars$mpg, mtcars$wt) # person's is the defafult. the person's is -0.87 indicating there is strong negative correlation

cor(mtcars$mpg, mtcars$wt, method="pearson")

cor(mtcars$mpg, mtcars$wt, method='pearson', use='complete.obs') # there is na, we can specify use=complete.obs so it will only use the rows that have complete observation

# to assess the correlation statistically significant, we can use correlation test
# H0= there is no association between the two variables
cor.test(mtcars$mpg, mtcars$wt) # reject the null hypo since the p-value is much less than 0.05


# compute multiple correlations
corr1 <- cor(mtcars)

corr1
melt(corr1)

# can we use pearson's correlation for iris petal length data?
shapiro.test(iris$petal_length) # the p-value is less than 0.05, the data do not meet the assumptions for parametric tests (e.g. normality), so we use a non parametric instead (the spearman or kendall)

# the most common method for computing non parametric correlation in literature is the spearman method
cor(iris$petal_length, iris$petal_width, method="spearman") # strong positive correlation

cor(iris$petal_length, iris$petal_width, method="kendall")

