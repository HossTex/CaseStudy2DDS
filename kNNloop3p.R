
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
AF$isFrequentTravel <- ifelse(AF$BusinessTravel == "Travel_Frequently",1,0)
AF$isSales <- ifelse(AF$Department == "Sales",1,0)
AF$isHumanResource <- ifelse(AF$EducationField == "Human Resources",1,0)
AF$MIbyE <- AF$MonthlyIncome/AF$Education
AF$isJI1 <- ifelse(AF$JobInvolvement == 1,1,0)
AF$isJRSR <- ifelse(AF$JobRole == "Sales Representative",1,0)


# Scale inputs
AF$ScaleAge <- scale(AF$Age)
AF$ScaleEducation = scale(AF$Education)
AF$ScaleDistanceFromHome = scale(AF$DistanceFromHome)
AF$ScaleEnvironmentSatisfaction = scale(AF$EnvironmentSatisfaction)
AF$ScaleGender <- scale(as.numeric(AF$Gender))
AF$ScaleHourlyRate <- scale(AF$HourlyRate)
AF$ScaleJobInvolvement = scale(AF$JobInvolvement)
AF$ScaleJobLevel = scale(AF$JobLevel)
AF$ScaleMonthlyIncome = scale(AF$MonthlyIncome)
AF$ScaleMonthlyIncomeLog = scale(log(AF$MonthlyIncome))
AF$ScaleMonthlyRate = scale(AF$MonthlyRate)
AF$ScaleMonthlyRateLog = scale(log(AF$MonthlyRate))
AF$ScaleNumCompaniesWorked = scale(AF$NumCompaniesWorked)
AF$ScaleOverTime = scale(as.numeric(AF$OverTime))
AF$ScalePercentSalaryHike = scale(AF$PercentSalaryHike)
AF$ScalePerformanceRating = scale(AF$PerformanceRating)
AF$ScaleRelationshipSatisfaction = scale(AF$RelationshipSatisfaction)
AF$ScaleStockOptionLevel = scale(AF$StockOptionLevel)
AF$ScaleTotalWorkingYears = scale(AF$TotalWorkingYears)
AF$ScaleTrainingTimesLastYear = scale(AF$TrainingTimesLastYear)
AF$ScaleWorkLifeBalance = scale(AF$WorkLifeBalance)
AF$ScaleYearsAtCompany = scale(AF$YearsAtCompany)
AF$ScaleYearsSinceLastPromotion = scale(AF$YearsSinceLastPromotion)
AF$ScaleYearsWithCurrManager = scale(AF$YearsWithCurrManager)

#colnames(AF)

AFColNam <- c("ScaleOverTime","isJL1","isSingle","ScaleMonthlyIncome","SO30","isTWY03","ScaleYearsWithCurrManager")

dfRes <- data.frame(Attribute1 = character(), Attribute2 = character(), Attribute3 = character(),
                    k=numeric(),Accuracy=numeric(),Sensitivity=numeric(),
                    Specificity=numeric(),stringsAsFactors = FALSE)

#Setup begin end list for 
testOffset = 0

EndA1 <- length(AFColNam) - 2 - testOffset
EndA2 <- length(AFColNam) - 1 - testOffset
EndA3 <- length(AFColNam) - testOffset

scaleFactor = 1

for (A1 in seq(1,EndA1)){
  BeginA2 = A1 + 1
  for (A2 in seq(BeginA2,EndA2)){
    BeginA3 = A2 + 1
    for (A3 in seq(BeginA3,EndA3)){
    
      testParam <- c(AFColNam[A1],AFColNam[A2],AFColNam[A3])
      
      #loop Outer loop for k inner loop for multiple passes
      for (k in seq(5,30)){
        # intialize results row
        
        Acc = 0
        Sens = 0
        Spec = 0
        
        # run model 10 times
        for (i in seq(1,(100/scaleFactor))){
          AFAttrit <- knn.cv(AF[,testParam], AF$Attrition, k = k)
          CM <- confusionMatrix(table(AFAttrit, AF$Attrition))
          
          Acc <- Acc + unname(CM[[3]][c('Accuracy')])
          Sens <- Sens + unname(CM[[4]][c('Sensitivity')])
          Spec <- Spec + unname(CM[[4]][c('Specificity')])
        }  
        
        dfRes <- dfRes %>% add_row(Attribute1 = AFColNam[A1], Attribute2 = AFColNam[A2], Attribute3 = AFColNam[A3], k=k,
                                   Accuracy=Acc*scaleFactor, Sensitivity=Sens*scaleFactor, Specificity=Spec*scaleFactor)
      }
    }
  }
}

write.csv(dfRes, file = "knnOptimize3p.csv", row.names = FALSE, na="")
