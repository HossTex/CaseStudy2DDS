

library(tidyverse)
library(class)
library(e1071)
library(caret)

setwd("C:/Data/DS6306/Project2")

kNN2p <- read.csv("kNNOptimize2p.csv")


kNN2p %>% filter(Specificity > 20) %>% arrange(desc(Specificity)) %>% 
  select(Attribute1,Attribute2) %>% distinct(Attribute1) 

kNN2p %>% filter(Specificity > 20) %>% arrange(desc(Specificity)) %>% 
  select(Attribute1,Attribute2) %>% distinct(Attribute2) 


kNN2p %>% filter(Specificity > 20)  %>% distinct(Attribute1, Attribute2, Specificity) %>% 
  arrange(desc(Specificity))

reskNN <- kNN2p %>% filter(Specificity > 20)  %>% group_by(Attribute1, Attribute2, Specificity) %>% 
  count(n = n()) %>% arrange(desc(Specificity))

print(reskNN)

ScaleOverTime
isJL1
isSingle
ScaleMonthlyIncome
SO30
isTWY03
ScaleYearsWithCurrManager


reskNN <- dfRes %>% filter(Specificity > 30)  %>% group_by(Attribute1, Attribute2, Attribute3, Specificity) %>% 
  count(n = n()) %>% arrange(desc(Specificity))


for (i in seq(1,10)){
  AFAttrit <- knn.cv(AF[,testParam], AF$Attrition, k = k)
  CM <- confusionMatrix(table(AFAttrit, AF$Attrition))
  
  Acc <- Acc + unname(CM[[3]][c('Accuracy')])
  Sens <- Sens + unname(CM[[4]][c('Sensitivity')])
  Spec <- Spec + unname(CM[[4]][c('Specificity')])
}  

dfRes <- dfRes %>% add_row(Attribute1 = AFColNam[A1], Attribute2 = AFColNam[A2],
                           k=k, Accuracy=Acc*10, Sensitivity=Sens*10 ,Specificity=Spec*10)



testParam <- c("ScaleOverTime","isSingle","ScaleMonthlyIncome","isJL1")

AFAttrit <- knn.cv(AF[,testParam], AF$Attrition, k = 11)
CM <- confusionMatrix(table(AFAttrit, AF$Attrition))

names(getModelInfo())
