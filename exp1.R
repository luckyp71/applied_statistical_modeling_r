require(dplyr)
require(datasets)
require(mice)
require(VIM)

# -------------------------- Indexing and Subseting of Dataset
View(airquality)

names(airquality) <- tolower(gsub('\\.','_',names(airquality)))

View(airquality)

airquality[1:2,c(-1,-5)]

vars <- names(airquality) %in% c('day')

head(airquality[vars])


head(airquality[!vars])


View(iris)
names(iris) <- tolower(gsub('\\.', '_', names(iris)))

View(iris)

df_iris = subset(iris, iris$species=='setosa')
View(df_iris)

# ------------------------- Data Cleaning
str(airquality)

glimpse(airquality)

summary(airquality)

??airquality

# drop na
test1 <- na.omit(airquality)

glimpse(test1)
summary(test1)

# impute na with arbitrary value
test2 <- airquality
test2[is.na(airquality)] <- 0
glimpse(test2)

# impute with mean
mean_ozone <- airquality%>%
  summarise(mean(ozone, na.rm=T))%>%
  pull()

test3 <- airquality
glimpse(test3)

test3 <- test3%>%
  mutate(ozone=ifelse(is.na(ozone), mean_ozone, ozone))

glimpse(test3)
summary(test3)

# ------------------------ Visualize the patterns o NAs
test4 <- airquality
md.pattern(test4) # 111 obs with no na values

mp <- aggr(test4, col=c('navyblue', 'yellow'),
           numbers=T, sortVars=T,
           labels=names(test4), cex.axis=.7,
           gap=3, ylab=c('Missing Data', 'Pattern'))


# ----------------------- Imputation with predictive mean mapping
# 500 iterations of predictive mean mapping for imputing
# 5 datasets
im_test4 <- mice(test4, m=5, maxit = 50, method='pmm', seed=500)

summary(im_test4)

head(im_test4$imp$ozone) # values imputed in ozone

# map back to complete dataset
completed_data <- complete(im_test4, 1)
head(completed_data)
glimpse(completed_data)
























