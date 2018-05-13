# Important library 
library(ggplot2)
library(foreign)
library(dplyr)
library(Hmisc)
library(randomForest)
library(caret)


#cleaning Environment
rm(list = ls())

#load the datasets
dataset_german_train = read.spss(file.choose(), to.data.frame = TRUE)
dataset_newapplicant_test = read.spss(file.choose(), to.data.frame = TRUE)

head(dataset_german_train)
#structure of dataset
str(dataset_german_train)
describe(dataset_german_train)
describe(dataset_newapplicant_test)


table(dataset_german_train$CREDIT)

#checking missing value 
colname <- colnames(dataset_german_train)
missing_value <- sapply(dataset_german_train, function(x){sum(is.na(x))})
missing_value_table <- data.frame(missing_value,colname,row.names = NULL)

#checking class of each varibale 
sapply(dataset_german_train, function(x){class(x)})

#check the relationship between CREDIT_USE and CREDIT in percentage
table(dataset_german_train$CREDIT_USE,dataset_german_train$CREDIT)*100/nrow(dataset_german_train)
# percent table between History and CREDIT 
table(dataset_german_train$HISTORY,dataset_german_train$CREDIT)*100/nrow(dataset_german_train)

#Convert target variable into binary 
dataset_german_train$CREDIT <- as.factor(ifelse(dataset_german_train$CREDIT=="GOOD",1,0))
#as.character(dataset_german_train$CREDIT)
#dataset_german_train$CREDIT[dataset_german_train$CREDIT=="GOOD"]<-"1"
#dataset_german_train$CREDIT[dataset_german_train$CREDIT=="BAD"]<-"0"

#plot of amount with respect to credit
ggplot(data = dataset_german_train,aes(x=AMOUNT,color=CREDIT)) + geom_density() +theme_minimal()
ggplot(data = dataset_german_train,aes(y=AMOUNT,x=CREDIT,color=CREDIT)) + geom_boxplot() +theme_minimal()
#plot of duration with respect to credit
ggplot(data = dataset_german_train,aes(x=DURATION,color=CREDIT)) + geom_density() +theme_minimal()
ggplot(data = dataset_german_train,aes(y=DURATION,x=CREDIT,color=CREDIT)) + geom_boxplot() +theme_minimal()

as.factor(dataset_german_train$NUM_CREDITS)
as.factor(dataset_german_train$NUM_DEPENDENTS)

as.factor(dataset_newapplicant_test$NUM_CREDITS)
as.factor(dataset_newapplicant_test$NUM_DEPENDENTS)

#str(dataset_german)

  #dataset_german_train <- subset(dataset_german_train, dataset_german_train$DURATION<45)
  #  dataset_newapplicant_test <- subset(dataset_newapplicant_test, dataset_newapplicant_test$DURATION<45)


#table(dataset_german_subset1$CREDIT)
table(dataset_german_train$CREDIT)
#remove customer id
dataset_german_train$CUST<- NULL
dataset_newapplicant_test$CUST<-NULL


#Perform hypothesis tests to see if 1) there is a relationship between checking account
#status and defaults and 2) there is a relationship between loan use and default.


library(gmodels)
tabl1 <- table(dataset_german_train$CREDIT_USE, dataset_german_train$CREDIT)
tabl1
chi1 = chisq.test(dataset_german_train$CREDIT_USE, dataset_german_train$CREDIT)
chi1
#p-value is 2.821e-05 which is less than 0.05, so there is relationship between Credit_Use and Credit

tabl2 <- table(dataset_german_train$HISTORY, dataset_german_train$CREDIT)
tabl2
chi2 = chisq.test(dataset_german_train$HISTORY, dataset_german_train$CREDIT)
chi2
#p-value is 2.821e-05 which is less than 0.05, so there is relationship between Hstory and Credit

#check the relationship between RADIOTV and CREDIT
tabl3 <- table(dataset_german_train$RADIOTV, dataset_german_train$CREDIT)
tabl3
chi3 = chisq.test(dataset_german_train$RADIOTV, dataset_german_train$CREDIT)
chi3
#p-value is 0.000952 which is less than 0.05, so there is relationship between Credit_Use and Credit

#check the relationship between Checking Account Status and CREDIT
tabl4 <- table(dataset_german_train$CHK_ACCT, dataset_german_train$CREDIT)
tabl4
chi4 = chisq.test(dataset_german_train$CHK_ACCT, dataset_german_train$CREDIT)
chi4
#p-value is  2.2e-16 which is less than 0.05, so there is relationship between Credit_Use and Credit


library(caTools)
set.seed(123)# random seed
# Splitting the dataset into train and Validation datasets
split = sample.split(dataset_german_train$CREDIT, SplitRatio = 0.8) # it returns true if the observation goes to training set, if it goes to test set, then it returns false
training_set = subset(dataset_german_train, split == TRUE)
test_set = subset(dataset_german_train, split == FALSE)


#Applying random forest on trainning data set
rf<- randomForest(CREDIT~.,data=training_set,ntree = 230,importance = TRUE,mtry=6)
rf
summary(rf)
importance(rf)
varImpPlot(rf,
           sort = T,
           main="Variable Importance",
           n.var=20)
plot(rf)
#variable Importance table
var.imp <- data.frame(importance(rf, type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]


rf_predicted<-predict(rf,newdata=test_set[,-32])
confusionMatrix(table(rf_predicted,test_set$CREDIT))
# we are getting 76% accuracy using random forest


dataset_newapplicant_test$pred <- predict(rf, newdata=dataset_newapplicant_test, type='response')
  dataset_newapplicant_test$pred <- as.factor(ifelse(dataset_newapplicant_test$pred==1,"GOOD","BAD"))
table(dataset_newapplicant_test$pred)*100/nrow(dataset_newapplicant_test)

ggplot(data=dataset_newapplicant_test,aes(x=pred,fill=pred))+geom_bar()
