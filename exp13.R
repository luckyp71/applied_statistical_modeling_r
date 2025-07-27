# ----------------------- Multi-collinearity
install.packages('mlbench')

require(datasets)
require(dplyr)
require(caret)
require(ggplot2)
require(plotly)
require(car)
require(mlbench)
require(corrplot)

# Handlecar# Handle multi-collinearity, i.e. presence of highly correlated predictors (x)
# use 0.7 as a threshold off multi-collinearity

data("BostonHousing")

data <- BostonHousing
head(data)

# drop response variable (y) for calculating multicollinearity
data_v2 <- data%>%
  select(c(-medv))%>%
  select(where(is.numeric))

head(data_v2)

data_cor <- cor(data_v2)
print(data_cor)

corrplot(data_cor)

# check highly correlated variables with corresponding threshold
highly_correlated <- findCorrelation(data_cor, cutoff = 0.7)
print(highly_correlated)
highly_cor_col <- names(data_v2)[highly_correlated]
print(highly_cor_col)

data3 <- data_v2%>%
  select(c(-highly_cor_col))

highly_cor_col

head(data3)
dim(data3)
dim(data_v2)

cor(data3)

# Choose a Variance Inflation Factor (VIF) cutoff under which a variable is retained (zuur et al. 2010)
# vif > 10 multi-collinearity
# can also reject predictors with vf 5-10
fit <- lm(medv~., data=data)
summary(fit)
vif(fit)

df <- cbind(BostonHousing$medv, data3)
df <- as.data.frame(df)
names(df) <- c('medv','crim', 'zn', 'rm', 'age', 'rad', 'ptratio', 'b', 'lstat')
head(df)
fit2 <- lm(medv~.,data=df)
summary(fit2)

vif(fit2)
