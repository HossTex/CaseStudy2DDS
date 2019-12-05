
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

trainIndices = sample(seq(1:length(AF$Age)),round(.8*length(AF$Age)))
trainAF = AF[trainIndices,]
testAF = AF[-trainIndices,]

modelAFTrain = naiveBayes(trainAF[,c("OverTime","DistanceFromHome")],trainAF$Attrition,laplace = 1)
table(predict(modelAFTrain,testAF[,c("OverTime","DistanceFromHome")]),testAF$Attrition)
CM = confusionMatrix(table(predict(modelAFTrain,testAF[,c("OverTime","DistanceFromHome")]),testAF$Attrition))

modelAFTrain = naiveBayes(trainAF[,c("Age","Education")],trainAF$Attrition)
table(predict(modelAFTrain,testAF[,c("Age","Education")]),testAF$Attrition)
CM = confusionMatrix(table(predict(modelAFTrain,testAF[,c("Age","Education")]),testAF$Attrition))

modelAFTrain = naiveBayes(trainAF[,c("OverTime","Education")],trainAF$Attrition)
table(predict(modelAFTrain,testAF[,c("OverTime","Education")]),testAF$Attrition)
CM = confusionMatrix(table(predict(modelAFTrain,testAF[,c("OverTime","Education")]),testAF$Attrition))

modelAFTrain = naiveBayes(trainAF[,c("OverTime","JobInvolvement")],trainAF$Attrition)
table(predict(modelAFTrain,testAF[,c("OverTime","JobInvolvement")]),testAF$Attrition)
CM = confusionMatrix(table(predict(modelAFTrain,testAF[,c("OverTime","JobInvolvement")]),testAF$Attrition))

modelAFTrain = naiveBayes(trainAF[,c("JobSatisfaction","JobInvolvement","RelationshipSatisfaction")],trainAF$Attrition)
table(predict(modelAFTrain,testAF[,c("JobSatisfaction","JobInvolvement","RelationshipSatisfaction")]),testAF$Attrition)
CM = confusionMatrix(table(predict(modelAFTrain,testAF[,c("JobSatisfaction","JobInvolvement","RelationshipSatisfaction")]),testAF$Attrition))

modelAFTrain = naiveBayes(trainAF[,c("StockOptionLevel","JobLevel","JobInvolvement","RelationshipSatisfaction")],trainAF$Attrition)
table(predict(modelAFTrain,testAF[,c("StockOptionLevel","JobLevel","JobInvolvement","RelationshipSatisfaction")]),testAF$Attrition)
CM = confusionMatrix(table(predict(modelAFTrain,testAF[,c("StockOptionLevel","JobLevel","JobInvolvement","RelationshipSatisfaction")]),testAF$Attrition))

# Best So far
modelAFTrain = naiveBayes(trainAF[,c("OverTime","Education","SO30","isSingle")],trainAF$Attrition)
table(predict(modelAFTrain,testAF[,c("OverTime","Education","SO30","isSingle")]),testAF$Attrition)
CM = confusionMatrix(table(predict(modelAFTrain,testAF[,c("OverTime","Education","SO30","isSingle")]),testAF$Attrition))

modelAFTrain = naiveBayes(trainAF[,c("OverTime","SO30","isSingle")],trainAF$Attrition,laplace = 1)
table(predict(modelAFTrain,testAF[,c("OverTime","SO30","isSingle")]),testAF$Attrition)
CM = confusionMatrix(table(predict(modelAFTrain,testAF[,c("OverTime","SO30","isSingle")]),testAF$Attrition))

modelAFTrain = naiveBayes(trainAF[,c("ScaleOverTime","isSingle","ScaleMonthlyIncome")],trainAF$Attrition,laplace = 1)
table(predict(modelAFTrain,testAF[,c("ScaleOverTime","isSingle","ScaleMonthlyIncome")]),testAF$Attrition)
CM = confusionMatrix(table(predict(modelAFTrain,testAF[,c("ScaleOverTime","isSingle","ScaleMonthlyIncome")]),testAF$Attrition))


a <- c("ScaleOverTime","isSingle","ScaleMonthlyIncome","isJL1")

trainControl <- trainControl(method = "cv", number = 10)

x = trainAF[,c("OverTime","SO30","isSingle")]
y = trainAF$Attrition

modelAFTrain <- train(x = x, 
                      y = y, 
                      method = "nb",
                      trControl = trainControl)

CM = confusionMatrix(table(predict(modelAFTrain,testAF[,c("OverTime","SO30","isSingle")]),testAF$Attrition))


# set up tuning grid
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)

# train model
nb.m2 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = trainControl,
  tuneGrid = search_grid,
  preProc = c("BoxCox", "center", "scale", "pca")
)

# top 5 modesl
nb.m2$results %>% 
  top_n(5, wt = Kappa) %>%
  arrange(desc(Kappa))


nb.m2$prob
nb.m2$method
nb.m2$bestTune
nb.m2$metric
nb.m2$control
nb.m2$predictors
nb.m2$grid


CM = confusionMatrix(table(predict(nb.m2,testAF[,c("OverTime","SO30","isSingle")]),testAF$Attrition))

resGrid <- expand.grid(
  usekernel = c(FALSE),
  fL = 4,
  adjust = 1
)


leastAcc <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = trainControl,
  tuneGrid = resGrid,
  preProc = c("BoxCox", "center", "scale", "pca")
)


NewleastAcc <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = trainControl,
  usekernel = c(FALSE),
  fL = 1,
  adjust = 1,
  preProc = c("BoxCox", "center", "scale", "pca")
)
getModelInfo(model="nb")

CM = confusionMatrix(table(predict(leastAcc,testAF[,c("OverTime","SO30","isSingle")]),testAF$Attrition))

unname(CM[[3]][c('Accuracy')])
unname(CM[[4]][c('Sensitivity')])
unname(CM[[4]][c('Specificity')])

