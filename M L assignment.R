install.packages("kernlab")
install.packages("e1071")
install.packages("NeuralNetTools")
install.packages("C50")
install.packages("cluster")
install.packages("arules")
install.packages("caret")
install.packages("ROCR")
install.packages("rpart")
install.packages("M etrics")
install.packages("rpart.plot")
install.packages("randomForest")

bankdata<-read.csv(file.choose())

table(bankdata$y)

prop.table(table(bankdata$y)) #table proportion

str(mydata)

class(bankdata)

edit(bankdata)

str(bankdata)

unique(bankdata$job)

table(bankdata$job)

bankdata1<-subset(bankdata,subset=(job!="unknown"))

table(bankdata$education)

bankdata2<-subset(bankdata,subset=(education!="unknown"))

table(bankdata2$education)

table(bankdata2$contact)

table(bankdata2$default)

barplot(table(bankdata$default))

table(bankdata2$poutcome=="unknown")
table(bankdata2$poutcome)

bankdata3<-subset(bankdata2, select=-c(default, duration, poutcome, contact))
table(bankdata3$y)

bankdata3$yvar<-ifelse(bankdata3$y=="yes",1,2)
table(bankdata3$yvar)

write.csv(bankdata3,"E:/Data/finalbankdata.csv")

#convert yvar from numeric to factor

bankdata3$yvar<-as.factor(bankdata3$yvar)
bankdata4<-subset(bankdata3, select=-c(y))

table(bankdata4$yvar)
NROW(bankdata4)

table(bankdata4$pdays)
unique(table(bankdata4$pdays))

barplot(table(bank$yvar))
hist(bankdata4$age)

install.packages("ggplot2")
library(ggplot2)

ggplot()+geom_bar(data=bankdata4, aes(x=(bankdata4$age), fill=factor(bankdata4$yvar)), position="fill")
str(bankdata4)

ggplot()+geom_bar(data=bankdata4, aes(x=(bankdata4$age), fill=factor(bankdata4$marital)), position="fill")


#modelling
set.seed(123) # to generate random numbers 
NROW(bankdata4) #measure data
datamixed=bankdata4[order(runif(43354)), ]  #shuffle data
traindata=datamixed[1:30235, ] #train data 70%
testdata=datamixed[30236:43354, ] #test data 30%

install.packages("C50")
library(C50)

modelc5<-C5.0(traindata$yvar~., data=traindata, trials=100, rules=TRUE) #decidion tree using c5.0 algorithm

#predict from test data using model

predictedc5=predict(modelc5, testdata[,1:12]) #all rows, 1:12 column(left last column which is yvar)
plot(predictedc5)

plot(modelc5)

#create confusion matrix to test accuracy of model(test and train) if accuracy and sensitivity is more. 
#if the model is not accurate, solve it by data level or algorithm level
install.packages("caret")
install.packages("e1071")
library(caret)

confusionMatrix(predictedc5,testdata[,13])

#reduce class data from train data

table(traindata$yvar) #see proportion

#change it to different proportion

#ordering train data 

orderdata<-traindata[order(-xtfrm(traindata$yvar)),] #xtrfm to order factor variables since its not a ordinal value.
table(orderdata$yvar)
barplot(table(orderdata$yvar))
sampletraindata1<-orderdata[18656:30235,]
barplot(table(sampletraindata1$yvar))


library(nnet)
nuralnet1<-nnet(traindata$yvar~.,size=7,data=traindata)
library(NeuralNetTools)
NeuralNetTools::plotnet(nuralnet1)
