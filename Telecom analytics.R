rm(list = ls())
setwd("C:/Users/ADMIN/Desktop/Telecom Analytics")
getwd()

library(dplyr)
library(Hmisc)
library(timeSeries)
library(ggplot2)


# Read Data

custm_id_data = read.csv("C:/Users/ADMIN/Desktop/Telecom Analytics/Telecom analytics_CustDemo.csv",header = TRUE, stringsAsFactors = FALSE, na.strings = c(""," ",NA))
custm_call_data = read.csv("Telecom analytics_WeeklyData.csv",header = TRUE,stringsAsFactors = FALSE,na.strings =c(""," ",NA))


# Data overview

head(custm_call_data)
head(custm_id_data)

str(custm_call_data)
str(custm_id_data)
describe(custm_call_data)
colnames(custm_call_data)
colnames(custm_id_data)

#changing column name 
colnames(custm_id_data)[colnames(custm_id_data)=="CustID"] <- "CustomerID"
# data_frame<- setNames(data_frames, "old-name", "new-name")
#joining the data 
custm_detail = merge(custm_id_data,custm_call_data,by="CustomerID",all=TRUE)

#library(sqldf)
# df1 = sqldf("select custm_call_data.CustomerID,Age,Gender,Pincode,Active,week,Calls,Minutes,Amt 
 #      from custm_id_data inner join custm_call_data 
  #     on custm_call_data.CustomerID=custm_id_data.CustomerID")

colnames(custm_detail)
summary(custm_detail)
str(custm_detail)

dim(custm_detail)
class(custm_detail)

##missing values Summary  
variable_names <- colnames(custm_detail)
Na_count <- sapply(custm_detail,function(x){sum(is.na(x))})
miss_sum <- data.frame(variable_names,Na_count,row.names = NULL)
View(miss_sum)
# convert chr into factor (Gender,Active)

custm_detail$Gender<-as.factor(custm_detail$Gender)
custm_detail$Active<-as.factor(custm_detail$Active)

str(custm_detail)

#defining new variable amout per call 

custm_detail$amt_per_call <- custm_detail$Amt/custm_detail$Calls
summary(custm_detail$amt_per_call)

#ploting data
ggplot(data = custm_detail,aes(y=amt_per_call,x=Gender))+geom_boxplot()
#boxplot(custm_detail$amt_per_call)

table(custm_detail$Gender)
barplot(table(custm_detail$Gender))

barplot(table(custm_detail$Active))


summary(custm_detail$Week)

ggplot(data=custm_detail) + geom_histogram(aes(x=Age), bin=5)+facet_wrap(~ Active)

ggplot(data=custm_detail) + geom_histogram(aes(x=Calls), bin=5)

ggplot(data=custm_detail) + geom_histogram(aes(x=Week), bin=30)+facet_wrap(~ Gender)

ggplot(data=custm_detail) + geom_dotplot(aes(x=Week,y=Calls))


plot(custm_detail$Week,custm_detail$Amt)

df2 = sqldf("select week,sum(Calls) as total_calls,sum(Minutes) as total_minutes,sum(Amt) as total_amt 
       from custm_detail group by Week")

plot(df2$Week,df2$total_amt)
plot(df2$Week,df2$total_calls)
plot(df2$Week,df2$total_minutes)



library(sqldf)
df3 = sqldf("select CustomerID,Age,count(week) as num_week,agebreak,Gender,sum(Calls) as total_calls,sum(Minutes) as total_minutes,sum(Amt) as total_amt 
       from custm_detail group by CustomerID")

df4 = sqldf("select week,sum(Calls) as total_calls,sum(Minutes) as total_minutes,sum(Amt) as total_amt 
       from custm_detail group by week")

df6<- custm_detail %>%group_by(Week) %>% summarise(total_minutes = sum(Minutes),total_calls=sum(Calls))
ggplot(data = df6, aes(x = Week,y=total_minutes)) + 
  geom_bar(stat = "identity") 


df7<- custm_detail %>%group_by(agebreak) %>% summarise(total_minutes = sum(Minutes),total_calls=sum(Calls))
ggplot(data = df7, aes(x = agebreak,y=total_minutes)) + 
  geom_bar(stat = "identity")

df8<- custm_detail %>%group_by(Week,agebreak) %>% summarise(total_minutes = sum(Minutes),total_calls=sum(Calls))

ggplot(data = df8, aes(x = Week,y=total_minutes,color=agebreak,fill=agebreak)) + 
  geom_bar(stat = "identity",position=position_dodge())


df9<- custm_detail %>%group_by(Gender,Week,agebreak) %>% summarise(total_minutes = sum(Minutes),total_calls=sum(Calls))

ggplot(data = df9, aes(x = Week,y=total_minutes,fill=Gender)) + 
  geom_bar(stat = "identity",position=position_dodge())
ggplot(data = df9, aes(x = agebreak,y=total_minutes,fill=Gender)) + 
  geom_bar(stat = "identity",position=position_dodge())


df10<- custm_detail %>%group_by(Gender,Week,agebreak) %>% summarise(avg_minutes = sum(Minutes)/sum(Calls))
ggplot(data = df10, aes(x = Week,y=avg_minutes,fill=Gender)) + 
  geom_bar(stat = "identity",position=position_dodge())

ggplot(data = df10, aes(x = agebreak,y=avg_minutes,fill=Gender)) + 
  geom_bar(stat = "identity",position=position_dodge())

df3$agebreak = cut(df3$Age, breaks=c(0,20, 30, 40,50,60,70), right = FALSE)

custm_detail$amt_per_call <- custm_detail$Amt/custm_detail$Calls
  
df3$amt_per_call <- df3$total_amt/df3$total_calls
  
plot(df3$total_minutes,df3$total_amt)
plot(df3$total_calls,df3$total_minutes)
plot(df3$agebreak,df3$amt_per_call)
barplot(table(df3$agebreak))

ggplot(data = custm_detail) + geom_boxplot(aes(x =Week,y=Calls,fill=Gender))

ggplot(data = custm_detail, aes(x=Gender,y=Amt)) + geom_boxplot()

custm_detail$agebreak = cut(custm_detail$Age, breaks=c(0,20, 30, 40,50,60,70), right = FALSE)


ggplot(data = custm_detail, aes(x=agebreak,y=Calls)) + geom_boxplot(aes(fill= Gender))
ggplot(data = custm_detail, aes(x=agebreak,y=amt_per_call)) + geom_boxplot(aes(fill= Gender))


ggplot(data = custm_detail, aes(x=agebreak,y=Minutes)) + geom_boxplot(aes(fill= Gender))

ggplot(data = custm_detail, aes(x=agebreak,y=Calls)) + geom_boxplot(aes(fill= Week))


table(custm_detail$Week)
table(custm_detail$agebreak)
