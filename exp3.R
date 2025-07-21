# ---------------------------- Measures of central
require(dplyr)
require(ggplot2)
require(plotly)
require(moments)
require(sciplot)
require(Mass)

x <- rnorm(n=1000, mean=3, sd=.25)

v <- ggplot(data.frame(x=x), aes(x=x))+
  geom_histogram(aes(fill='orange'))+
  theme_minimal()

ggplotly(v)

median(x)
mean(x)
sd(x)
skewness(x)
# the data is a bit skew to the right, the mean is greater than median and the skewness is about 0.05

# ---------------------------- Measures of Variation
sd(iris$sepal_length, na.rm=T)
se(iris$sepal_length, na.rm=T)

data <- ToothGrowth

View(data)

v <- ggplot(data, aes(x=supp, y=len))+
  geom_boxplot(aes(color=supp))+
  theme_minimal()

ggplotly(v)


str(ChickWeight)
View(ChickWeight)

names(ChickWeight) <- tolower(names(ChickWeight))

data <- ChickWeight

str(data)
View(data)

v <- data%>%
  ggplot(aes(x=time, y=weight))+
  geom_boxplot(aes(group=time, color=diet))+
  facet_grid(. ~ diet)+
  theme_minimal()+
  theme(legend.position = 'none')

ggplotly(v)

# ------------------------ visualizing discrete data
data <- tapply(iris$sepal_length, iris$species, mean)
df <- data.frame(
  species=names(data),
  mean_sepal_length=as.numeric(data)
)


View(df)

v <- df%>%
  ggplot(aes(x=species, y=mean_sepal_length))+
  geom_col(aes(fill=species))+
  theme_minimal()+
  theme(legend.position = 'none')

ggplotly(v)


titanic = as.data.frame(Titanic)
names(titanic) <- tolower(names(titanic))

View(titanic)

v <- titanic%>%
  ggplot(aes(x=age, weight=freq))+
  geom_bar(aes(fill=age))+
  facet_grid(sex~class)+
  theme_minimal()+
  theme(legend.position='none')

ggplotly(v, tooltip = c('x','y'))


isum = iris%>%
  group_by(species)%>%
  summarise(
    avg_pl = mean(petal_length, na.rm=T),
    sd_pl = sd(petal_length, na.rm=T)
  )

isum

v <- isum%>%
  ggplot(aes(x=species, y=avg_pl,
             text=paste0(
               "Species: ", species,
               "<br>Avg: ", round(avg_pl, 2),
               "<br>Lower Bound Error: ", round(avg_pl-sd_pl,2),
               "<br>Upper Bound Error: ", round(avg_pl+sd_pl,2)
             )))+
  geom_bar(aes(fill=species), stat = 'identity')+ # should use stat=identity to ensure barplot using the value as-is rather than doing a count
  geom_errorbar(aes(ymin=avg_pl-sd_pl, ymax=avg_pl+sd_pl))+
  theme_minimal()+
  theme(legend.position='none')

ggplotly(v, tooltip = 'text')


men <- c(150, 120, 45)
women <- c(320, 270, 100)
rbind(men,women)

food_survey <- as.data.frame(rbind(men,women))
names(food_survey) <- c('chicken', 'salad', 'cake')

food_survey

chisq.test(food_survey)
# the p-value is greater than 0.05, hence we can conclude there is no association between gender and food

survey <- MASS::survey
str(survey)
names(survey) <- tolower(gsub('\\.', '_', names(survey)))
View(survey)
levels(survey$smoke)
sfreq = table(survey$smoke)
sfreq

factor(c('male','male','female','male'))
