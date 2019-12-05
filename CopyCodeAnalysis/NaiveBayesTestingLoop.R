

library(tidyverse)
library(class)
library(e1071)
library(caret)

AF <- read.csv("CaseStudy2-data.csv")

AF$MIbyE <- AF$MonthlyIncome/AF$Education
AF$isSingle <- ifelse(AF$MaritalStatus == "Single",1,0)
AF$SO30 <- ifelse(AF$StockOptionLevel == 3 | AF$StockOptionLevel == 0,1,0)
AF$isAgeLE22 <- ifelse(AF$Age <= 22,1,0)
AF$isJL1 <- ifelse(AF$JobLevel == 1,1,0)
AF$isJI1 <- ifelse(AF$JobInvolvement == 1,1,0)

c("ScaleOverTime","ScaleMonthlyIncome")

trainIndices = sample(seq(1:length(AF$Age)),round(.8*length(AF$Age)))
trainAF = AF[trainIndices,]
testAF = AF[-trainIndices,]


# Best So far
modelAFTrain = naiveBayes(trainAF[,c("OverTime","Education","SO30","isSingle")],trainAF$Attrition)
table(predict(modelAFTrain,testAF[,c("OverTime","Education","SO30","isSingle")]),testAF$Attrition)
CM = confusionMatrix(table(predict(modelAFTrain,testAF[,c("OverTime","Education","SO30","isSingle")]),testAF$Attrition))

modelAFTrain = naiveBayes(trainAF[,c("OverTime","SO30","isSingle")],trainAF$Attrition,laplace = 1)
table(predict(modelAFTrain,testAF[,c("OverTime","SO30","isSingle")]),testAF$Attrition)
CM = confusionMatrix(table(predict(modelAFTrain,testAF[,c("OverTime","SO30","isSingle")]),testAF$Attrition))


m <- naiveBayes(Attrition ~., data = AF)
pred <- predict(m, AF)
CM = confusionMatrix(table(pred, AF$Attrition))

m <- naiveBayes(Attrition ~., data = trainAF)
pred <- predict(m, testAF)
CM = confusionMatrix(table(pred, testAF$Attrition))


colnames(AF)

m <- naiveBayes(Attrition ~., data = trainAF, laplace = 1)
pred <- predict(m, testAF)
CM = confusionMatrix(table(pred, testAF$Attrition))

formula <- "Attrition ~ isSingle + isAgeLE22 + isJI1 + OverTime + JobRole"

m <- naiveBayes(formula = formula, data = trainAF, laplace = 1)
pred <- predict(m, testAF)
CM = confusionMatrix(table(pred, testAF$Attrition))

m <- naiveBayes(Attrition ~ isSingle + isAgeLE22 + isJI1 + OverTime + JobRole, data = trainAF, laplace = 1)
pred <- predict(m, testAF)
CM = confusionMatrix(table(pred, testAF$Attrition))


m <- naiveBayes(Attrition ~ 
                  #Age + 
                  #BusinessTravel + 
                  #DailyRate + 
                  #Department + 
                  DistanceFromHome + 
                  #Education + 
                  EducationField + 
                  #EmployeeCount + 
                  #EnvironmentSatisfaction + 
                  #Gender +
                  #HourlyRate + 
                  #JobInvolvement + 
                  #JobLevel + 
                  JobRole + 
                  JobSatisfaction + 
                  #MaritalStatus + 
                  MonthlyIncome + 
                  #MonthlyRate + 
                  #NumCompaniesWorked + 
                  OverTime + 
                  #PercentSalaryHike + 
                  PerformanceRating + 
                  #RelationshipSatisfaction + 
                  StockOptionLevel + 
                  TotalWorkingYears + 
                  TrainingTimesLastYear + 
                  #WorkLifeBalance + 
                  #YearsAtCompany + 
                  YearsInCurrentRole + 
                  #YearsSinceLastPromotion + 
                  #YearsWithCurrManager + 
                  MIbyE + 
                  isSingle + 
                  SO30
                  #isAgeLE22 + 
                  #isJL1 + 
                  #isJI1
                  ,
                  data = trainAF, laplace = 1)
pred <- predict(m, testAF)
confusionMatrix(table(pred, testAF$Attrition))



modelAFTrain = naiveBayes(trainAF[,c("ScaleOverTime","isSingle","ScaleMonthlyIncome")],trainAF$Attrition,laplace = 1)
table(predict(modelAFTrain,testAF[,c("ScaleOverTime","isSingle","ScaleMonthlyIncome")]),testAF$Attrition)
CM = confusionMatrix(table(predict(modelAFTrain,testAF[,c("ScaleOverTime","isSingle","ScaleMonthlyIncome")]),testAF$Attrition))


