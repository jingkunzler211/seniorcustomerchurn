install.packages("lmtest")
install.packages("sandwich")


rm(list = ls())

library("sandwich")
library("lmtest")

setwd("/Users/jingk/OneDrive/Desktop/R Working Directory")
mydata<-read.csv("churn_6.csv",header = T)

#split the data
split<-sample.split(mydata,SplitRatio = 0.8)
split
train<-subset(mydata,split=="TRUE")
test<-subset(mydata,split=="FALSE")

lmapp <- lm(Churn ~ SeniorCitizen + Partner + Dependents + Payment_ElectronicCheck + Payment_BankTransfer +
              Payment_MailedCheck + Payment_CreditCard +
              PaperlessBilling + SeniorCitizen * Partner + SeniorCitizen * Dependents +
              SeniorCitizen * Payment_ElectronicCheck + SeniorCitizen * Payment_BankTransfer + SeniorCitizen * Payment_MailedCheck +
              SeniorCitizen * Payment_CreditCard + SeniorCitizen * PaperlessBilling, data=train)
summary(lmapp)

train$Churn.fit <- predict(lmapp)
plot(Churn ~ train$Churn.fit, data = train)

##In order to conduct hypothesis tests and confidence intervals for the marginal effects an explanatory variable
##has on the outcome variable, we must first correct for heteroskedasticity
##All LPM Models are heteroskedastic
plot(y=lmapp$residuals^2, x=lmapp$fitted.values,
     ylab="Squared Residuals", xlab="Predicted probabilities")

# the White estimator for correcting heteroskedasticity
# compute the White heteroskedastic variance/covariance matrix for the coefficients with the call to vcovHC
vv <- vcovHC(lmapp, type="HC1")

# Then we call coeftest() to use this estimate for the variance / covariance to properly compute our standard
# errors, t-statistics, and p-values for the coefficients.
coeftest(lmapp, vcov = vv)

##predict on train set
res_train <-predict(lmapp,train,type="response")

confmatrix_train <-table(Actual_value=train$Churn, Predicted_value= res_train > 0.5)
confmatrix_train

#Accuracy
(confmatrix_train[[1,1]]+confmatrix_train[[2,2]])/sum(confmatrix_train)

##predict on test data
#Run the test data through the model
res_test <-predict(lmapp,test,type="response")

#Validate the model - Confusion Matrix
confmatrix_test <-table(Actual_value=test$Churn, Predicted_value= res_test > 0.5)
confmatrix_test

#Accuracy
(confmatrix_test[[1,1]]+confmatrix_test[[2,2]])/sum(confmatrix_test)
