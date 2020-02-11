library(caTools)

setwd("/Users/aileenpan/Documents/Schoolwork_Baruch/2019 Fall/9660-Data Mining/Group Project")
mydata<-read.csv("Churn.csv",header = T)

#split the data
split<-sample.split(mydata,SplitRatio = 0.8)
split
train<-subset(mydata,split=="TRUE")
test<-subset(mydata,split=="FALSE")

#Munge the data
mydata$Churn<-as.factor(mydata$Churn)
mydata$SeniorCitizen<-as.factor(mydata$SeniorCitizen)
mydata$Partner<-as.factor(mydata$Partner)
mydata$Dependents<-as.factor(mydata$Dependents)
mydata$PaymentMethod<-as.factor(mydata$PaymentMethod)
mydata$PaperlessBilling<-as.factor(mydata$PaperlessBilling)

#Train th model using the training data
mymodel<-glm(Churn~SeniorCitizen + Partner + Dependents + PaymentMethod + PaperlessBilling, data=train, family = 'binomial')
summary(mymodel)

#Run the test data through the model
res<-predict(mymodel,test,type="response")

res<-predict(mymodel,train,type="response")

#Validate the model - Confusion Matrix
confmatrix<-table(Actual_value=train$Churn, Predicted_value= res > 0.5)
confmatrix

#Accuracy
(confmatrix[[1,1]]+confmatrix[[2,2]])/sum(confmatrix)