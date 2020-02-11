rm(list = ls())

##set seed
set.seed(123)

##read data as dataSet
dataSet <- read.csv("C:/Users/jingk/OneDrive/Desktop/R Working Directory/telco2.csv")

##split the dataset 80-20
trainIndex  <- sample(1:nrow(dataSet), size = round(0.8*nrow(dataSet)), replace = F)

trainset <- dataSet[trainIndex, ]
valset <- dataSet[-trainIndex, ]

##model1: Churn ~ . 

model1 <- glm(Churn ~., data = trainset, family = "binomial")
summary(model1)

##VIF for multicollinearity
install.packages("regclass")
library(regclass)
VIF(model1)

##predict on valset
pred1 <- predict(model1, valset, type ="response")

##misclassification rate
misClassError(valset$Churn, pred1, threshold = 0.5)

##model2 : only InternetService
model2 <- glm(Churn ~ InternetService, data = trainset, family = "binomial")
summary(model2)
pred2 <- predict(model2, valset, type = "response")

##test model using original data: telco1
data2 <- telco1
trainIndex2  <- sample(1:nrow(data2), size = round(0.8*nrow(data2)), replace = F)

train <- data2[trainIndex2, ]
validate <- data2[-trainIndex2, ]

fit <- glm(Churn ~ InternetService + SeniorCitizen + Partner, data = data2, family = "binomial")
summary(fit)
pred_test <- predict(fit, validate, type = "response")

y_pred_num <- ifelse(pred_test> 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- validate$Churn
mean(y_pred == y_act)
