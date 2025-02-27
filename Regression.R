

chooseCRANmirror()

house.df <- read.csv("RTP_housing_price.csv")
View(house.df)
str(house.df)


levels(as.factor( house.df$city) )

house.df<-house.df[-4351,]
house.df<-house.df[-4347,]
house.df <- na.omit(house.df)
str(house.df)  # structure of the data frame



house.df$price<- ifelse(house.df$price==0,0.0001,house.df$price)
install.packages("dplyr")
install.packages("psych")
library(dplyr)
library(psych)

describe(house.df)


##### partition data into training and validation data
set.seed(1)

train.index <- sample( c( 1:length(house.df[,1]) ), 0.6*length(house.df[,1]) )  



selected.var <- c(2,3,4,5,6, 7, 8, 9, 10, 11,12,13)

## create training data
train.df <- house.df[train.index, selected.var]
## create validation data
valid.df <- house.df[-train.index, selected.var]    # "-" works for numbers; otherwise setdiff

str(train.df)
str(valid.df)


house.lm <- lm(price ~ . , data = train.df)    # lm can account for categorical variables

# display regression results
summary(house.lm)

## check the normality of residuals
hist(resid(house.lm))
# QQ plot of residuals: another check of residuals' normality 
qqnorm(resid(house.lm))   # QQ plot of residuals
qqline(resid(house.lm))  # to check if points fall nicely onto the line


#### get accuracy measures of the regression model in the training and validationd data
install.packages("forecast")
library(forecast)
# use predict() to make predictions Y^ on the validation set. 
house.lm.pred <- predict(house.lm, valid.df)
# display accuracy measures  for the validation data
accuracy(house.lm.pred, valid.df$price)

# compare to the accuracy measures  for the training data
house.lm.pred.t <- predict(house.lm, train.df)
accuracy(house.lm.pred.t, train.df$price)



#### Perform variable selection

# use regsubsets() in package leaps to run an exhaustive search. 
# like lm, categorical predictors are accepted
# if needed, can be turned into dummies manually using  e.g. factor(  )

install.packages("leaps")
library(leaps)

# variable selection using "regsubsets"
search <- regsubsets(price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "exhaustive")

#graphical display of selection results, based on adjusted R-sq
plot(search, scale="adjr2")  

#graphical display of selection results, based on BIC
# BIC is an extension of AIC=-2lnL + 2*k; prefer small AIC
# plot(search)  # default is using BIC

str(train.df)

### create the indicator variables in the train.df and valid.df 
# if there is categorical variable, to shows how many levels in Fuel_Type

str(valid.df)
###


# Now estimate the model chosen by regsubsets:

# can run the model with the variables you want to include using "+", and don't want to include using "-"
#    now, since Fuel_Type and the 3 indicators are both in the data frame "train.df", 
#    remove the Fuel_Type which is not needed now and add the levels you want to include, e.g.

house.lm <- lm( price ~ bedrooms + bathrooms +sqft_living + sqft_lot + floors + 
                  waterfront + view + condition +sqft_above + sqft_basement, data = train.df)  
summary(house.lm)


# alternatively, you can run the model with everything in train.df using "." and use "-" to exclude
# the variables you don't want to include, e.g.,
#  since Fuel_Type and the 3 indicators are both in the data frame "train.df",
#  remove the original categorical variable Fuel_Type and the levels you don't want to included, e.g.

house.lm <- lm( price ~ . -yr_built, data = train.df)     
summary(house.lm)

# if you don't have categorical variable and no indicators are created, simply use "-" to 
# exclude the variables you don't want to include, e.g.,
# house.lm <- lm( Price ~ . -Met_Color  , data = train.df)     
# summary(house.lm)


## check the normality of residuals
hist(resid(house.lm))
# QQ plot of residuals: another check of residuals' normality 
qqnorm(resid(house.lm))   # QQ plot of residuals
qqline(resid(house.lm))  # to check if points fall nicely onto the line

#### get accuracy measures of the regression model in the training and validationd data
# install.packages("forecast")
# library(forecast)
# use predict() to make predictions Y^ on the validation set. 
house.lm.pred <- predict(house.lm, valid.df)
accuracy(house.lm.pred, valid.df$price)
# compare to the accuracy measures  for the training data
house.lm.pred.t <- predict(house.lm, train.df)
accuracy(house.lm.pred.t, train.df$price)



#### sometime taking log of Y may improve the normality of residuals, e.g.,
house.lm <- lm(log(price) ~ . -yr_built, data = train.df)     
summary(house.lm)

