
## Libraries
library(tidyverse)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)
library(GGally)
library(keras)
library(future.apply)
library(randomForest)
library(mgcv)
library(gbm)
library(xgboost)
library(glmnet)
library(caret)
library(nnet)
library(snowfall)

mg.data = read.csv("/Users/zubiamansoor/Desktop/MG_DATASET_ST.csv", header = T)
customer.data = read.csv("/Users/zubiamansoor/Desktop/CUST_DATASET_ST.csv", header = T)
fsa.data = read.csv("/Users/zubiamansoor/Desktop/FSA_DATASET_ST.csv", header = T)

head(mg.data)
head(customer.data)
head(fsa.data)

str(mg.data)
str(customer.data)
str(fsa.data)

# check missing data

apply(is.na(mg.data[,-10]), 2, which) #no missing data
apply(is.na(customer.data), 2, which) #no missing data 

# create a new column 

# Split train, val and test
train.mg = mg.data[which(mg.data$Sample=="Estimation"),]
val.mg = mg.data[which(mg.data$Sample=="Validation"),]
test.mg = mg.data[which(mg.data$Sample=="Holdout"),]


# Plot
data_to_plot = mg.data
#ggpairs(data_to_plot[,-2], ggplot2::aes(color="teal")) +
  #ggtitle("Scatterplot Matrix") +
  #theme(plot.title = element_text(hjust = 0.5))

# Random Forest

train.data.rf = mg.data[which(mg.data$Sample!="Holdout"),]
train.data.rf2 = mg[which(mg$Sample!="Holdout"),]
train.data.rf2$amort_period = as.factor(train.data.rf2$amort_period)
n = nrow(train.data.rf)
V=5
set.seed(301404)
folds = floor((sample.int(n)-1)*V/n) + 1

#rf = randomForest(default ~., data = train.data.rf[, -c(1:3, 11)], 
                  #ntree=200, mtry=2, importance=TRUE)
#plot(rf) # 200 looks fine, maybe make it 500
#pred.rf = predict(rf, newdata = train.data.rf)
#train.err = mean(ifelse(pred.rf == train.data.rf$default, 
            #yes=0, no=1))
#table(pred.rf, train.data.rf$default)

# Fit default RF model
sMSE = matrix(NA, ncol=1, nrow = 5)
CV.err = matrix(NA, ncol=1, nrow = 5)
for(v in 1:V){
  rf = randomForest(as.factor(default) ~., data = train.data.rf[, -c(1:3, 11)], 
                    ntree=200, mtry=2, importance=TRUE)
  pred1 = predict(rf, newdata = train.data.rf[folds!=v,])
  pred2 = predict(rf, newdata = train.data.rf[folds==v,])
  sMSE[v,1] = mean(ifelse(pred1 == train.data.rf[folds!=v,"default"], 
                          yes=0, no=1))
  CV.err[v,1] = mean(ifelse(pred2 == train.data.rf[folds==v,"default"], 
                            yes=0, no=1))
}

CV.err
sMSE 


## CARET

set.seed(301404887)

folds <- 5
cvIndex <- createFolds(factor(train.data.rf$default), folds, returnTrain = T)
tc <- trainControl(index = cvIndex,
                   method = 'cv', 
                   number = folds)
  
rf.grid <- expand.grid(mtry = c(5, 10, 15, 20))
  
rf.model <- train(as.factor(default) ~., data = train.data.rf[, -c(1:3, 11)], 
                  method="rf", ntrees = 200,
                    trControl = tc, tuneLength=15)
  
print(rf.model)

load(file = "mg_clean.rds")  
  
#### ROC

set.seed(301404887)

folds <- 5
cvIndex <- createFolds(factor(train.data.rf$default), folds, returnTrain = T)
tc <- trainControl(index = cvIndex,
                   method = 'cv', 
                   number = folds, classProbs = TRUE)


rf.model <- train(as.factor(default) ~., data = train.data.rf[, -c(1:3, 11)], 
                  method="rf", ntrees = 200,
                  trControl = tc, tuneLength=15, metric = "ROC")

print(rf.model)

#### ROC

set.seed(301404887)

folds <- 5
cvIndex <- createFolds(factor(train.data.rf$default), folds, returnTrain = T)
tc <- trainControl(index = cvIndex,
                   method = 'cv', 
                   number = folds)


rf.model <- train(as.factor(default) ~., data = train.data.rf2[, -c(1:3, 11)], 
                  method="rf", ntrees = 200,
                  trControl = tc, tuneLength=15)

print(rf.model)
