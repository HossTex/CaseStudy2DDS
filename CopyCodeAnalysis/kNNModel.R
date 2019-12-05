
library(tidyverse)
library(class)
library(caret)

setwd("C:/Data/DS6306/Project2")

AF <- read.csv("CaseStudy2-data.csv")

AF$isSingle <- ifelse(AF$MaritalStatus == "Single",1,0)
AF$isSalesRepresentative <- ifelse(AF$JobRole == "Sales Representative",1,0)
AF$isNumCmpWorked <- ifelse(AF$NumCompaniesWorked == 1 | AF$NumCompaniesWorked > 4,1,0)
AF$isAgeLE22 <- ifelse(AF$Age <= 22,1,0)
AF$isJL1 <- ifelse(AF$JobLevel == 1,1,0)
AF$SO30 <- ifelse(AF$StockOptionLevel == 3 | AF$StockOptionLevel == 0,1,0)
AF$isTWY03 <- ifelse(AF$TotalWorkingYears <=3,1,0)


# Scale inputs
AF$ScaleOverTime = scale(as.numeric(AF$OverTime))
AF$ScalePercentSalaryHike = scale(AF$PercentSalaryHike)
AF$ScaleStockOptionLevel = scale(AF$StockOptionLevel)
AF$ScaleEducation = scale(AF$Education)
AF$ScaleMonthlyIncome = scale(AF$MonthlyIncome)
AF$ScaleJobLevel = scale(AF$JobLevel)
AF$ScaleMonthlyIncome = scale(AF$MonthlyIncome)
AF$ScaleYearsAtCompany = scale(AF$YearsAtCompany)
AF$ScaleTotalWorkingYears = scale(AF$TotalWorkingYears)
AF$ScaleDistanceFromHome = scale(AF$DistanceFromHome)
AF$ScaleJobInvolvement = scale(AF$JobInvolvement)

# Best So far

kNNParamBest <- c("ScaleYearsAtCompany","isSingle","ScaleEducation","ScaleOverTime","ScaleStockOptionLevel")
AFAttrit <- knn.cv(AF[,kNNParamBest], AF$Attrition, k = 15)
confusionMatrix(table(AFAttrit, AF$Attrition))

kNNParam <- c("ScaleYearsAtCompany","isSingle","ScaleEducation","ScaleOverTime","ScaleStockOptionLevel")
AFAttrit <- knn.cv(AF[,kNNParam], AF$Attrition, k = 15)
confusionMatrix(table(AFAttrit, AF$Attrition))


# Loop kNN model from k = 5 to 30 X 100
# Define results data frame

dfRes <- data.frame(k=numeric(),Accuracy=numeric(),Sensitivity=numeric(),Specificity=numeric(),stringsAsFactors = FALSE)

#loop Outer loop for k inner loop for multiple passes
for (k in seq(5,30)){
  # intialize results row
  dfRes <- dfRes %>% add_row(k=k, Accuracy=0,Sensitivity=0,Specificity=0)

    # run model 100 times
  for (i in seq(1,100)){
    AFAttrit <- knn.cv(AF[,kNNParam], AF$Attrition, k = k)
    CM <- confusionMatrix(table(AFAttrit, AF$Attrition))
    
    dfRes$Accuracy[dfRes$k==k] <- dfRes$Accuracy[dfRes$k==k] + unname(CM[[3]][c('Accuracy')])
    dfRes$Sensitivity[dfRes$k==k] <- dfRes$Sensitivity[dfRes$k==k] + unname(CM[[4]][c('Sensitivity')])
    dfRes$Specificity[dfRes$k==k] <- dfRes$Specificity[dfRes$k==k] + unname(CM[[4]][c('Specificity')])
  }  
}

dfRes


dfRes$Accuracy[k==k]

AFAttrit <- knn.cv(AF[,c("isTWY03","ScaleYearsAtCompany","isSingle","ScaleEducation","ScaleOverTime","ScaleStockOptionLevel")], 
                   AF$Attrition, k = 19)
confusionMatrix(table(AFAttrit, AF$Attrition))

unname(CM[[3]][c('Accuracy')])
unname(CM[[4]][c('Sensitivity')])
unname(CM[[4]][c('Specificity')])


StockOptionLevel

