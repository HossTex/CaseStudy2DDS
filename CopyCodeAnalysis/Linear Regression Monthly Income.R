

library(tidyverse)
library(dplyr)
library(caret)


AF <- read.csv("CaseStudy2-data.csv")
head(AF)

AF %>% ggplot(aes(y = MonthlyIncome, x = OverTime)) + geom_point()

trainIndices = sample(seq(1:length(AF$Age)),round(.80*length(AF$Age)))
trainAF = AF[trainIndices,]
testAF = AF[-trainIndices,]


fit = lm(MonthlyIncome~YearsAtCompany + TotalWorkingYears + JobLevel, data = trainAF)
summary(fit)

predictions <- predict(fit, testAF)
RMSE(predictions, testAF$MonthlyIncome)

fit <-  train(MonthlyIncome~YearsAtCompany + TotalWorkingYears + JobLevel, method = "lm", data = AF, trControl = trainControl(method = "LOOCV"))
fit$results[2]
summary(fit$finalModel)

predictions <- predict(fit, testAF)

predictions
