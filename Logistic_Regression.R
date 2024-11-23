house.df <- read.csv("RatanTejaPunati_housing_price.csv")
View(house.df)
str(house.df)

levels(as.factor( house.df$city) )

house.df<-house.df[-4351,]
house.df<-house.df[-4347,]
house.df <- house.df[house.df$price != 0, ]
house.df <- na.omit(house.df)
View(house.df)
str(house.df)



install.packages("dplyr")
install.packages("psych")
library(dplyr)
library(psych)


describe(house.df)

house.df$price <- ifelse(house.df$price > mean(house.df$price), 1, 0)  
str(house.df)


set.seed(2)


train.index <- sample( c(1:dim(house.df)[1]), dim(house.df)[1]*0.6 ) 

selected.var <- c(2,3,4,5,6, 7, 8, 9, 10, 11,12,13,14)

train.df <- house.df[train.index, selected.var]
valid.df <- house.df[-train.index, selected.var]

str(train.df)
str(valid.df)





logit.reg <- glm(price ~ ., data = train.df, family = "binomial") 
summary(logit.reg)



install.packages("caret")
library(caret)    # Classfication And REgression Training

varImp(logit.reg)
plot( varImp(logit.reg) )


pred <- predict(logit.reg, valid.df, type = "response")    
c.mat <- table(ifelse(pred > 0.5, 1, 0), valid.df$price)    #2 way table: row by column
c.mat

sum(diag(c.mat))/sum(c.mat)
c.mat[2,2]/sum(c.mat[,2])
c.mat[1,1]/sum(c.mat[,1]) 


pred <- predict(logit.reg, train.df, type = "response")

c.mat <- table(ifelse(pred > 0.5, 1, 0), train.df$price)    #2 way table: row by column
c.mat

sum(diag(c.mat))/sum(c.mat)
c.mat[2,2]/sum(c.mat[,2])
c.mat[1,1]/sum(c.mat[,1])


install.packages("MASS")  
library(MASS)

logit.reg <- glm(price ~ ., data = train.df, family = "binomial") 

step<-stepAIC(logit.reg)   #stepwise 
step$anova

logit.reg <- glm(price ~ . - sqft_basement- sqft_lot- waterfront, data = train.df, family = "binomial") 
summary(logit.reg)

library(caret) 
varImp(logit.reg)
plot( varImp(logit.reg) )


logit.reg <- glm(price ~ bedrooms + bathrooms +sqft_living + floors + 
                    view + condition +sqft_above + yr_built+yr_renovated, data = train.df, family = "binomial") 
summary(logit.reg)


pred <- predict(logit.reg, valid.df, type = "response")    

c.mat <- table(ifelse(pred > 0.5, 1, 0), valid.df$price)    #2 way table: row by column
c.mat   # row: predicted ;   column: actual 

sum(diag(c.mat))/sum(c.mat) 
c.mat[2,2]/sum(c.mat[,2])  
c.mat[1,1]/sum(c.mat[,1])   

######## classification matrix: training data
pred <- predict(logit.reg, train.df, type = "response")

c.mat <- table(ifelse(pred > 0.5, 1, 0), train.df$price)    #2 way table: row by column
c.mat
sum(diag(c.mat))/sum(c.mat) # this gives accuracy: overall correct classification percentage 
c.mat[2,2]/sum(c.mat[,2])   # this gives True positive percentage, or sensitivity
c.mat[1,1]/sum(c.mat[,1])   # this gives True negatie percentage, or specificity



###### Plotting  ROC-curve using "pROC" library  ######
# install some libraries first 
install.packages("e1071")
library(e1071)   # misc functions in stat, prob
install.packages("Rcpp")
library(Rcpp)    # R and C++ integration
install.packages("pastecs")
library(pastecs)


install.packages("pROC")
library(pROC)

#ROC-curve for validation data
pred <- predict(logit.reg, valid.df, type = "response")    
roc_score=roc(valid.df[,8], pred)                #AUC score  
plot(roc_score ,main ="ROC curve -- Logistic Regression ")
# names(roc_score)
roc_score$auc

#ROC-curve for training data
pred <- predict(logit.reg, train.df, type = "response")
roc_score=roc(train.df[,8], pred) 
plot(roc_score ,main ="ROC curve -- Logistic Regression ")
roc_score$auc

