
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

trainIndices = sample(seq(1:length(AF$Age)),round(.75*length(AF$Age)))
trainAF = AF[trainIndices,]
testAF = AF[-trainIndices,]

AFColNam <- colnames(AF)

#Remove columns we dont need to train on
SkipName <- c("ID","Attrition","EmployeeCount","EmployeeNumber","Over18","StandardHours")

AFColNam <- AFColNam[!AFColNam %in% SkipName]

# Setup things used by model repeatedly

trainControl <- trainControl(method = "cv", number = 10)

# set up tuning grid
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)

y = trainAF$Attrition

#Setup begin end list for 
testOffset = 0

EndA1 <- length(AFColNam) - 1 - testOffset
EndA2 <- length(AFColNam) - testOffset

# Results dataframe

dfRes <- data.frame(Attribute1 = character(), Attribute2 = character(), 
                    Acc = numeric(), Sens = numeric(), Spec = numeric(),
                    stringsAsFactors = FALSE)

for (A1 in seq(1,EndA1)){
  BeginA2 = A1 + 1
  for (A2 in seq(BeginA2,EndA2)){

    testParam <- c(AFColNam[A1],AFColNam[A2])
    x = trainAF[,testParam]

    nb.m2 <- train(
      x = x,
      y = y,
      method = "nb",
      trControl = trainControl,
      tuneGrid = search_grid,
      preProc = c("BoxCox", "center", "scale") #, "pca"
    )

    CM = confusionMatrix(table(predict(nb.m2,testAF[,testParam]),testAF$Attrition))

    Acc <- unname(CM[[3]][c('Accuracy')])
    Sens <- unname(CM[[4]][c('Sensitivity')])
    Spec <- unname(CM[[4]][c('Specificity')])

    dfRes[nrow(dfRes) + 1,] <- c(AFColNam[A1],AFColNam[A2], Acc, Sens, Spec)
    
    prnStr <- paste(AFColNam[A1],AFColNam[A2])
    print(prnStr)
  }
}

write.csv(dfRes, file = "NBOptimize.csv", row.names = FALSE, na="")



