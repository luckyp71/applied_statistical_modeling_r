# ------------- Model Selection in Linear Regression
# Adjuster R2: Adjusted R2 also indicate how well the line fits a model,
# but adjusts for the number of terms in a model. If we add more and more
# useless variables to a model, adjusted r-squared will decrease and vice versa.

# Akaike Information Criterion (AIC) is a measure of the relative quality of statistical models
# for a given set of data. Select the model with the lowest AIC

# Forward selection involves starting with no xs and add new predictors one by one (testing model performance at each step).
# Backward selection involves starting with all model xs and removing xs with highest p-value one by one. 
# Stepwise regression combines both.

#install.packages('relaimpo')

require(dplyr)
require(datasets)
require(MASS)
require(relaimpo)

data <- iris
fit <- lm(sepal_length ~., data=data)
summary(fit)
step(fit, direction='both')
step(fit, direction='backward')

# include the performance of the null model as well
null <- lm(sepal_length~1, data=data) #mean value of y predicts new ys
full <- lm(sepal_length~., data=data) #include all xs

step(null, scope=list(lower=null, upper=full), direction='both')
# start from no predictor, the stepwise process includes predictors from the iris dataset,
# helping to decrease the AIC. This means that including all predictors improves the model's ability to predict the sepal length


# Bootstrap measures of relative importance (1000 samples)
# drawing randomly with replacement rom a set of data points
bootstrap <- boot.relimp(fit,
                         b=1000,
                         type=c("lmg", 
                                "last",
                                "first",
                                #"pratt"
                                ),
                         rank=T,
                         diff=T,
                         rela=T)

boot_eval<- booteval.relimp(bootstrap, sort=T)
boot_eval
plot(boot_eval) # petal length is the most importance variable
