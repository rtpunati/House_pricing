


###################   Classification Tree analysis ###################
### This code estimate 3 types of Classification Trees Models:
### (1) single classification tree, (2) random forest, (3) bagging method


##### install packages to be used

library(rpart)
library(rpart.plot)

library(adabag)


library(dplyr)       #for data wrangling
library(e1071)       #for calculating variable importance
library(caret)       #for general model fitting
library(rpart)       #for fitting decision trees
library(ipred)       #for fitting bagged decision trees


####### preparing data
house.df <- read.csv("RTP_housing_price.csv")
View(house.df)

str(house.df)


### if there are categorical variables in the data frame, say X1, X2,
### to shows how many levels in each categorical variable:
# levels(as.factor( house.df$X1) )
# levels(as.factor( house.df$X2) )


#### remove observations with missing values 
house.df<-house.df[-4351,]
house.df<-house.df[-4347,]
house.df <- house.df[house.df$price != 0, ]
house.df <- na.omit(house.df)
str(house.df)  # structure of the data frame


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


####### transform the target variable (price) into Factor (with 2 levels) to run the Tree models
house.df$price = as.factor(house.df$price)
View(house.df)
str(house.df)

#### If you have other categorial variables,
#### make sure all the categorical variables are converted into factors.
####  some examples:
# house.df$X1 = as.factor(house.df$X1)
# house.df$X2 = as.factor(house.df$X2)
# house.df$Education = as.factor(house.df$Education)
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
str(valid.df)



####### (1) classification tree model estimation: single classification tree   ######
###### single tree with all predictors: using default option to develop the classification tree
#### note: if there is problem with using all X's, use the significant X's chosen in LR model 

tr <- rpart(price ~ ., data = train.df)      # estimate a single tree using default options
prp(tr, type = 1, extra = 1, split.font = 1, varlen = -10)   # plot the tree
# color the terminal node in the plot
prp(tr, type = 1, extra = 1, split.font = 1, varlen = -10, 
    box.col=ifelse(tr$frame$var == "<leaf>", 'orange', 'white') )  

# variable importance
# names(tr)   # to see the names available in "tr"
tr$variable.importance    


#### change options to get a Deeper tree
#### lower cp (complexity parameter) and minsplit values make the tree split further.

##  tr <- rpart(price ~ ., data = train.df, method = "class", cp = 0, minsplit = 1)
##   prp(tr, type = 1, extra = 1, split.font = 1, varlen = -10, 
##   box.col=ifelse(tr$frame$var == "<leaf>", 'green', 'white'))  



### generate the classification matrix 
####classification matrix for training data
pred <- predict(tr, train.df, type = "class")

c.mat <-  table(pred, train.df$price)
c.mat   # row: predicted ;   column: actual 

sum(diag(c.mat))/sum(c.mat) # this gives accuracy: overall correct classification percentage 
c.mat[2,2]/sum(c.mat[,2])   # this gives True positive percentage, or sensitivity
c.mat[1,1]/sum(c.mat[,1])   # this gives True negatie percentage, or specificity

####classification matrix for validation data
pred <- predict(tr, valid.df, type = "class")

c.mat <-  table(pred, valid.df$price)
c.mat   # row: predicted ;   column: actual 

sum(diag(c.mat))/sum(c.mat) # this gives accuracy: overall correct classification percentage 
c.mat[2,2]/sum(c.mat[,2])   # this gives True positive percentage, or sensitivity
c.mat[1,1]/sum(c.mat[,1])   # this gives True negatie percentage, or specificity



####### (2) classification tree model estimation: random forest   ######

###### install package to run random forest
library(randomForest)

###### random forest with all predictors: using default option to develop the random forest
#### note: if there is problem with using all X's, use the significant X's chosen in LR model 
rf <- randomForest(price ~ ., data = train.df, ntree = 500, 
                   mtry = 4, nodesize = 5, importance = TRUE)  

##### to see the importance of predictors in the RF model, 
#####    use the variable importance plot command:
varImpPlot(rf, type = 1)

# importance(rf)

### generate the classification matrix 
####classification matrix for training data
rf.pred <- predict(rf, train.df, type = "class")

c.mat <- table(rf.pred, train.df$price)
c.mat   # row: predicted ;   column: actual 

sum(diag(c.mat))/sum(c.mat) # this gives accuracy: overall correct classification percentage 
c.mat[2,2]/sum(c.mat[,2])   # this gives True positive percentage, or sensitivity
c.mat[1,1]/sum(c.mat[,1])   # this gives True negatie percentage, or specificity

####classification matrix for validation data
rf.pred <- predict(rf, valid.df, type = "class")

c.mat <- table(rf.pred, valid.df$price)
c.mat   # row: predicted ;   column: actual 

sum(diag(c.mat))/sum(c.mat) # this gives accuracy: overall correct classification percentage 
c.mat[2,2]/sum(c.mat[,2])   # this gives True positive percentage, or sensitivity
c.mat[1,1]/sum(c.mat[,1])   # this gives True negatie percentage, or specificity


####### (3) classification tree model estimation: bagging method   ######

###### bagging method with all predictors: using default option to develop the bagging model
#### note: if there is problem with using all X's, use the significant X's chosen in LR model 

bag <- bagging(price ~ ., data = train.df)

###### install package to get varImp
install.packages("stats")
library(stats)

varImp(bag)

### generate the classification matrix 
####classification matrix for training data
bag.pred <- predict(bag, train.df, type = "class")

c.mat <- table(bag.pred, train.df$price)
c.mat   # row: predicted ;   column: actual 

sum(diag(c.mat))/sum(c.mat) # this gives accuracy: overall correct classification percentage 
c.mat[2,2]/sum(c.mat[,2])   # this gives True positive percentage, or sensitivity
c.mat[1,1]/sum(c.mat[,1])   # this gives True negatie percentage, or specificity

####classification matrix for validation data
bag.pred <- predict(bag, valid.df, type = "class")

c.mat <- table(bag.pred, valid.df$price)
c.mat   # row: predicted ;   column: actual 

sum(diag(c.mat))/sum(c.mat) # this gives accuracy: overall correct classification percentage 
c.mat[2,2]/sum(c.mat[,2])   # this gives True positive percentage, or sensitivity
c.mat[1,1]/sum(c.mat[,1])   # this gives True negatie percentage, or specificity
