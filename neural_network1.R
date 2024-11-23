
##### install packages to be used
library(neuralnet)
library(nnet)
library(caret)


####### preparing data
### read in data, in csv format
house.df <- read.csv("RatanTejaPunati_housing_price.csv")
View(house.df)
str(house.df)
house.df<-house.df[-4351,]
house.df<-house.df[-4347,]
house.df <- house.df[house.df$price != 0, ]
#house.df <- house.df[house.df$yr_renovated != 0, ]

house.df <- na.omit(house.df)
View(house.df)
str(house.df) 

### if there are categorical variables in the data frame, say X1, X2,
### to shows how many levels in each categorical variable:
# levels(as.factor( house.df$X1) )
# levels(as.factor( house.df$X2) )


#### remove observations with missing values 
 # structure of the data frame


#### if dataset is too large and want to select part of data for analysis
## e.g., if you want to use the first 1000 rows of data
# house.df <-house.df[1:1000, ]


##### summary statistics output 
library(dplyr)
library(psych)

describe(house.df)

##### If the target variable Y is not 1 or 0, 
##### convert it to 1 or 0, there are several possibilites:

#1   if the target variable Y is numerics and not in 1, 0,  
#  can convert  Y to     1 if Y> mean(Y),   and 0 otherwise.   E.g.:
house.df$price <- ifelse(house.df$price > mean(house.df$price), 1, 0)  
str(house.df)





#3   if the target variable Y is categorical with more than 2 classes, combine some
# of the classes and make it 2 classes, here is an example:
library(dplyr)



##########################################

#### check the data frame again to make sure the target variable Y is 1 or 0:
str(house.df)



##### if there are some variables you want to remove
house.df <- house.df[ , c( 2,3,4,5,6, 7, 8, 9, 10, 11,12,13,14) ]  # Drop ID and zip code in columns 1 and 5.
# alternatively,  
#  house.df <- house.df[ , -c(1, 5) ]    

View(house.df)
str(house.df)




###### normalizing the predictor variables using scale() 
### normalizing the predictor variables in house.df (except the target Y which is in column 8)  ######
### may skip this step if don't want to normalize,
### but normalization may makes the estimation runs faster
house.df[ , c(2:13)] <- scale(house.df[ ,c(2:13)])    # normalizing the predictors, NOT the target Y
View(house.df)
str(house.df)


#### create two logical variables Loan1 and Loan0 based on Y to run NN model, they are added to the end of the data frame
####  logiccal variables has value True or False

house.df$price1 <- house.df$price=="1"
house.df$price0 <- house.df$price=="0"

# class( house.df$Loan1)
# class( house.df$Loan0)

View(house.df)
str(house.df)


####### partition data into training and validation
set.seed(2)   # random sample can be reproduced by setting a value for seed

## sample 60% of data 
train.index <- sample(c(1:dim(house.df)[1]), dim(house.df)[1]*0.6)    # dim(house.df)[1] gives the no. of obs.

## create training data
train.df <- house.df[train.index, ]
## create validation data
valid.df <- house.df[-train.index, ]    # "-" works for numbers; otherwise setdiff

str(train.df)
str(train.df)


#######   NN model estimation: Full model and Best model   ######
####### depends on the no. of X's and observations, may take a longer time
####### Loan1 and Loan0 are 2 logical classes; can extend to multi class

##### (1): Full model 
##### Full model with one layer and 2 neurons (hidden = 2 is just for simplicity, can try more neurons)
##### Full model with all predictors may take a long time, and may not converge
nn <- neuralnet(price1 + price0 ~ . -price , data = train.df, linear.output = F, hidden = 2)
# the original target price is in the data frame but not used

### plot NN network
plot(nn, rep="best")

#### display NN weights
# names(nn)
nn$weights

### classification matrix: training data
pred <- predict(nn, train.df)

#### "pred" has 2 columns: prob in class 1; and prob in class 0
head(pred)
summary(pred)

### generate the classification matrix 
c.mat <- table(ifelse(pred[,1] > 0.5, 1, 0), train.df$price)    #2 way table: row by column
c.mat   # row: predicted ;   column: actual 

sum(diag(c.mat))/sum(c.mat) # this gives accuracy: overall correct classification percentage 
c.mat[2,2]/sum(c.mat[,2])   # this gives True positive percentage, or sensitivity
c.mat[1,1]/sum(c.mat[,1])   # this gives True negatie percentage, or specificity

#### classification matrix: validation data
pred <- predict(nn, valid.df)

c.mat <- table(ifelse(pred[,1] > 0.5, 1, 0), valid.df$price)    #2 way table: row by column
c.mat   # row: predicted ;   column: actual 

sum(diag(c.mat))/sum(c.mat) # this gives accuracy: overall correct classification percentage 
c.mat[2,2]/sum(c.mat[,2])   # this gives True positive percentage, or sensitivity
c.mat[1,1]/sum(c.mat[,1])   # this gives True negatie percentage, or specificity



####### (2): Model building: search for a better NN model
### You can use the best X's selected in Report 3 (Logistic Regression), and also based on your judgement, to select predictors.
###   the choice of X below is just an example.
###   
### 4 NN models are attempted below: one layer with 2, 5, 10 neurons; and two layers with 2,3 neurons
### you can try each and see if estimation converges, if not converge, try other specifications 
### (e.g. different numbers, different predictors, etc)
### In general, it takes longer to run for more complicated models

### in the following, the use of 2, 5, 10, (2,3) neurons are just example, you can change these numbers,
###  it depends on the size of the dataset, and how many predictors are used,
###  and also how long does it take to get the output, and if the model converges.

### These are trial and error method to get a better model, you can try more models
###  model performance based on classification matrix and ROC curve.

#  2a:  one layer 2 neurons
nn <- neuralnet(price1 + price0 ~ sqft_living + yr_built , data = train.df, linear.output = F, hidden = 2)
#  2b:  one layer 5 neurons
# nn <- neuralnet(Loan1 + Loan0 ~ Income + Education , data = train.df, linear.output = F, hidden = 5)
#  2c:  one layer 10 neurons
# nn <- neuralnet(Loan1 + Loan0 ~ Income + Education , data = train.df, linear.output = F, hidden = 10)
#  2d:  two layers with 3, 2 neurons
# nn <- neuralnet(Loan1 + Loan0 ~ Income + Education , data = train.df, linear.output = F, hidden = c(2,3)  )

#  you can try more models with other specifications 
# nn <- neuralnet(Loan1 + Loan0 ~ Income + Education +???? , data = train.df, linear.output = F, hidden = ???   )


# plot network
plot(nn, rep="best")

#### display NN weights
# names(nn)
nn$weights

##### classification matrix: training data
pred <- predict(nn, train.df)

head(pred)
summary(pred)

c.mat <- table(ifelse(pred[,1] > 0.5, 1, 0), train.df$price)    #2 way table: row by column
c.mat   # row: predicted ;   column: actual 

sum(diag(c.mat))/sum(c.mat) # this gives accuracy: overall correct classification percentage 
c.mat[2,2]/sum(c.mat[,2])   # this gives True positive percentage, or sensitivity
c.mat[1,1]/sum(c.mat[,1])   # this gives True negatie percentage, or specificity


##### classification matrix: validation data
pred <- predict(nn, valid.df)

c.mat <- table(ifelse(pred[,1] > 0.5, 1, 0), valid.df$price)    #2 way table: row by column
c.mat   # row: predicted ;   column: actual 

sum(diag(c.mat))/sum(c.mat) # this gives accuracy: overall correct classification percentage 
c.mat[2,2]/sum(c.mat[,2])   # this gives True positive percentage, or sensitivity
c.mat[1,1]/sum(c.mat[,1])   # this gives True negatie percentage, or specificity



###### Plotting  ROC-curve using "pROC" library  ######
library(pROC)

#ROC-curve for validation data
pred <- predict(nn, valid.df, type = "response")    
roc_score=roc(valid.df$price, pred[,1])                #AUC score  
plot(roc_score ,main ="ROC curve -- Neural Network ")
# names(roc_score)
roc_score$auc

#ROC-curve for training data
pred <- predict(nn, train.df, type = "response")
roc_score=roc(train.df$price, pred[,1]) #AUC score
plot(roc_score ,main ="ROC curve -- Neural Network ")
roc_score$auc

