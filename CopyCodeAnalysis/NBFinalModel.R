
library(tidyverse)
library(class)
library(e1071)
library(caret)

setwd("C:/Data/DS6306/Project2")

AF <- read.csv("CaseStudy2-data.csv")
AFnoAttrit <- read.csv("CaseStudy2CompSet No Attrition.csv")

AF$MIbyE <- AF$MonthlyIncome/AF$Education
AF$isSingle <- ifelse(AF$MaritalStatus == "Single",1,0)
AF$SO30 <- ifelse(AF$StockOptionLevel == 3 | AF$StockOptionLevel == 0,1,0)
AF$YCR02 <- ifelse(AF$YearsInCurrentRole <= 2,1,0)
AF$SO3 <- ifelse(AF$StockOptionLevel == 3,1,0)
AF$SO0 <- ifelse(AF$StockOptionLevel == 0,1,0)
AF$isJS1 <- ifelse(AF$JobSatisfaction == 1,1,0)
AF$isJS4 <- ifelse(AF$JobSatisfaction == 4,1,0)

trainIndices = sample(seq(1:length(AF$Age)),round(.8*length(AF$Age)))
trainAF = AF[trainIndices,]
testAF = AF[-trainIndices,]

# Evaluate in a loop Execute 100 times each for laplace 0 to 5

dfRes <- data.frame(laplace=numeric(),Accuracy=numeric(),Sensitivity=numeric(),
                    Specificity=numeric(),stringsAsFactors = FALSE)

for (lp in seq(0,5)){
  
  Acc = 0
  Sens = 0
  Spec = 0
  
  for (i in seq(1,100)){
#    set.seed(i+300)
    trainIndices = sample(seq(1:length(AF$Age)),round(.80*length(AF$Age)))
    trainAF = AF[trainIndices,]
    testAF = AF[-trainIndices,]
    
    m <- naiveBayes(Attrition ~ DistanceFromHome + EducationField + MonthlyIncome + JobRole + isJS4 + isJS1 + YCR02 +
                      OverTime + PerformanceRating + TotalWorkingYears + TrainingTimesLastYear + StockOptionLevel +
                      YearsInCurrentRole + MIbyE + isSingle + SO30 + SO3 + SO0 + MaritalStatus
                    ,data = trainAF, laplace = lp)
    pred <- predict(m, testAF)
    CM <- confusionMatrix(table(pred, testAF$Attrition))

    Acc <- Acc + unname(CM[[3]][c('Accuracy')])
    Sens <- Sens + unname(CM[[4]][c('Sensitivity')])
    Spec <- Spec + unname(CM[[4]][c('Specificity')])
    
  }

  dfRes <- dfRes %>% add_row(laplace=lp,Accuracy=Acc, Sensitivity=Sens, Specificity=Spec)

}

# Laplace seems to peak at at 3
m <- naiveBayes(Attrition ~ DistanceFromHome + EducationField + MonthlyIncome + JobRole + isJS4 + isJS1 + YCR02 +
                  OverTime + PerformanceRating + TotalWorkingYears + TrainingTimesLastYear + StockOptionLevel +
                  YearsInCurrentRole + MIbyE + isSingle + SO30 + SO3 + SO0 + MaritalStatus
                ,data = trainAF, laplace = 3)
pred <- predict(m, testAF)
CM <- confusionMatrix(table(pred, testAF$Attrition))

unname(CM[[3]][c('Accuracy')])
unname(CM[[4]][c('Sensitivity')])
unname(CM[[4]][c('Specificity')])

# prepare the Non Attrition test data
AFnoAttrit$MIbyE <- AFnoAttrit$MonthlyIncome/AFnoAttrit$Education
AFnoAttrit$isSingle <- ifelse(AFnoAttrit$MaritalStatus == "Single",1,0)
AFnoAttrit$SO30 <- ifelse(AFnoAttrit$StockOptionLevel == 3 | AFnoAttrit$StockOptionLevel == 0,1,0)
AFnoAttrit$isAgeLE22 <- ifelse(AFnoAttrit$Age <= 22,1,0)
AFnoAttrit$isJL1 <- ifelse(AFnoAttrit$JobLevel == 1,1,0)
AFnoAttrit$isJI1 <- ifelse(AFnoAttrit$JobInvolvement == 1,1,0)
AFnoAttrit$ScaleMonthlyIncome = scale(AFnoAttrit$MonthlyIncome)
AFnoAttrit$ScaleMonthlyIncomeLog = scale(log(AFnoAttrit$MonthlyIncome))
AFnoAttrit$YCR02 <- ifelse(AFnoAttrit$YearsInCurrentRole <= 2,1,0)


AFnoAttrit$SO3 <- ifelse(AFnoAttrit$StockOptionLevel == 3,1,0)
AFnoAttrit$SO0 <- ifelse(AFnoAttrit$StockOptionLevel == 0,1,0)

AFnoAttrit$isJR123 <- ifelse(AFnoAttrit$JobRole == "Technician Manager"| AFnoAttrit$JobRole == "Manufacturing Director"| AFnoAttrit$JobRole == "Research Director",1,2)
AFnoAttrit$isJR123 <- ifelse(AFnoAttrit$JobRole == "Sales Representative",3,AFnoAttrit$isJR123)

AFnoAttrit$isJS1 <- ifelse(AFnoAttrit$JobSatisfaction == 1,1,0)
AFnoAttrit$isJS4 <- ifelse(AFnoAttrit$JobSatisfaction == 4,1,0)

#Predict the results
prednoAttrit <- predict(m, AFnoAttrit)

write.csv(prednoAttrit, file = "Case2PredictionsRuthford Attrition.csv", row.names = FALSE, na="")
