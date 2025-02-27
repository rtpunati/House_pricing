house.df <- read.csv("RTP_housing_price.csv")
View(house.df)
str(house.df)


levels(as.factor( house.df$city) )
levels(as.factor( house.df$statezip) )
levels(as.factor( house.df$street) )

house.df <- na.omit(house.df)
house.df<-house.df[-4351,]
house.df<-house.df[-4347,]
str(house.df)  # structure of the data frame
View(house.df)
install.packages("dplyr")
install.packages("psych")
library(dplyr)
library(psych)

describe(house.df)
length(house.df[,1])


set.seed(1) 

train.index <- sample( c( 1:length(house.df[,1]) ), 0.6*length(house.df[,1]) )  


selected.var <- c(2,3,4,5,6, 7, 8, 9, 10, 11,12,13)

train.df <- house.df[train.index, selected.var]

valid.df <- house.df[-train.index, selected.var]

str(train.df)
View(train.df)
str(valid.df)

house.lm <- lm(price ~ . , data = train.df)

summary(house.lm)
hist(resid(house.lm))
qqnorm(resid(house.lm))   # QQ plot of residuals
qqline(resid(house.lm))  # to check if points fall nicely onto the line

install.packages("forecast")
library(forecast)

house.lm.pred <- predict(house.lm, valid.df)
accuracy(house.lm.pred, valid.df$price)



house.lm.pred.t <- predict(house.lm, train.df)
accuracy(house.lm.pred.t, train.df$price)

install.packages("leaps")
library(leaps)

search <- regsubsets(price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "exhaustive")

plot(search, scale="adjr2")  

house.lm <- lm( price ~ . -yr_built, data = train.df)    
summary(house.lm)

hist(resid(house.lm))

qqnorm(resid(house.lm))   # QQ plot of residuals
qqline(resid(house.lm))

house.lm.pred <- predict(house.lm, valid.df)
accuracy(house.lm.pred, valid.df$price)


house.lm.pred.t <- predict(house.lm, train.df)
accuracy(house.lm.pred.t, train.df$price)


log(10)
log(10000)


