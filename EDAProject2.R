
library(tidyverse)

AF <- read.csv("CaseStudy2-data.csv")

AF <- AF %>% mutate(AtrY = as.numeric(ifelse(AF$Attrition == "Yes", 1, 0)))
AF <- AF %>% mutate(lnHourlyRate = as.numeric(log(HourlyRate)))
AF <- AF %>% mutate(lnMonthlyRate = as.numeric(log(MonthlyRate)))
AF <- AF %>% mutate(lnMonthlyIncome = as.numeric(log(MonthlyIncome)))


AFage <- AF %>% group_by(Age) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFage, mapping = aes(x= Age, y=PecentAttrit, size=totalPeople)) +
         geom_point(stat = "identity")

AFBT <- AF %>% group_by(BusinessTravel) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFBT, mapping = aes(x= BusinessTravel, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

AFBT <- AF %>% group_by(Department) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFBT, mapping = aes(x= Department, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

# Maybe
AFDFH <- AF %>% group_by(DistanceFromHome) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFDFH, mapping = aes(x= DistanceFromHome, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

# linear negative more education the more stick
AFE <- AF %>% group_by(Education) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFE, mapping = aes(x= Education, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

# Human Resource and Technical Degrees
AFEF <- AF %>% group_by(EducationField) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFEF, mapping = aes(x= EducationField, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

# 1's are much more likely to leave
AFES <- AF %>% group_by(EnvironmentSatisfaction) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFES, mapping = aes(x= EnvironmentSatisfaction, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

#Male higher than female by 2%
AFG <- AF %>% group_by(Gender) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFG, mapping = aes(x= Gender, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

AFHR <- AF %>% group_by(HourlyRate) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFHR, mapping = aes(x= HourlyRate, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

AFHRE <- AF %>% group_by(HourlyRate, Education) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFHRE, mapping = aes(x= HourlyRate, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity") +
  facet_wrap(vars(Education), scales = "free", nrow = 2, shrink=TRUE)

# Strong negative linear relationship
AFJI <- AF %>% group_by(JobInvolvement) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFJI, mapping = aes(x= JobInvolvement, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

# Lowest Job level over a quarter leave
AFJL <- AF %>% group_by(JobLevel) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFJL, mapping = aes(x= JobLevel, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

# 45% of Sales Representatives leave
AFJR <- AF %>% group_by(JobRole) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFJR, mapping = aes(x= JobRole, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")
AF$JobRole
# Strong negative linear relationship
AFJS <- AF %>% group_by(JobSatisfaction) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFJS, mapping = aes(x= JobSatisfaction, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

#Single people highest turnover
AFMS <- AF %>% group_by(MaritalStatus) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFMS, mapping = aes(x= MaritalStatus, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity") + ggtitle("Marital Status versus Attrition")

# need to look at this with a histogram maybe two bars ???????
# AFMI <- AF %>% group_by(MonthlyIncome) %>% summarise(CountAttrit = sum(AtrY), totalPeople = n())
# ggplot(data = AFMI, mapping = aes(x= MonthlyIncome, y=CountAttrit)) +
#   geom_bar(stat = "identity")
# 
# # need to look at this with a histogram maybe two bars ???????
# AFMR <- AF %>% group_by(MonthlyRate) %>% summarise(CountAttrit = sum(AtrY), totalPeople = n())
# ggplot(data = AFMR, mapping = aes(x= MonthlyRate, y=CountAttrit)) +
#   geom_bar(stat = "identity")

# this could be significant Either having worked at only 1 or more than 4 much higher
AFNCW <- AF %>% group_by(NumCompaniesWorked) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFNCW, mapping = aes(x= NumCompaniesWorked, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

# very significant 10 verus over 30 percent
AFOT <- AF %>% group_by(OverTime) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFOT, mapping = aes(x= OverTime, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

# Above 21% very high turnover
AFPSH <- AF %>% group_by(PercentSalaryHike) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFPSH, mapping = aes(x= PercentSalaryHike, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

# Not much difference
AFPR <- AF %>% group_by(PerformanceRating) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFPR, mapping = aes(x= PerformanceRating, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

# lowest score has a >20% attrit
AFRS <- AF %>% group_by(RelationshipSatisfaction) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFRS, mapping = aes(x= RelationshipSatisfaction, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

# no help at all
AFSH <- AF %>% group_by(StandardHours) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFSH, mapping = aes(x= StandardHours, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

# no stock option very high turnover. Highest stock option second highest turnover
AFSOL <- AF %>% group_by(StockOptionLevel) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFSOL, mapping = aes(x= StockOptionLevel, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity") + ggtitle("Stock Option Level versus Attrition")

# don't see much other than zero working years is very high and slides down rapidly
AFTWY <- AF %>% group_by(TotalWorkingYears) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFTWY, mapping = aes(x= TotalWorkingYears, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

# Very similiar to TotalWorking Years
AFYAT <- AF %>% group_by(YearsAtCompany) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFYAT, mapping = aes(x= YearsAtCompany, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

# some greater attrition in very low years
AFYICR <- AF %>% group_by(YearsInCurrentRole) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFYICR, mapping = aes(x= YearsInCurrentRole, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

# doesn't look like anything
AFYSLP <- AF %>% group_by(YearsSinceLastPromotion) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFYSLP, mapping = aes(x= YearsSinceLastPromotion, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")

# other than zeros years are very high not much >30%
AFYWCM <- AF %>% group_by(YearsWithCurrManager) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFYWCM, mapping = aes(x= YearsWithCurrManager, y=PecentAttrit, size=totalPeople)) +
  geom_point(stat = "identity")


#


ggplot(data = AF, mapping = aes(x= MonthlyIncome, y=MonthlyRate, color=AtrY)) +
  geom_point()

ggplot(data = AF, mapping = aes(x= lnMonthlyIncome, y=lnMonthlyRate, color=AtrY)) +
  geom_point()


ColorTable <- c("red","green")


AFJLE <- AF %>% group_by(JobLevel, Education) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFJLE, mapping = aes(x= JobLevel, y=Education, color=PecentAttrit, size = totalPeople)) +
  geom_point() + scale_color_gradient(low = 'blue', high = 'red')

AFJLTWY <- AF %>% group_by(JobLevel, TotalWorkingYears) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFJLTWY, mapping = aes(x= JobLevel, y=TotalWorkingYears, color=PecentAttrit, size = totalPeople)) +
  geom_point() + scale_color_gradient(low = 'blue', high = 'red')

AFJLYAC <- AF %>% group_by(JobLevel, YearsAtCompany) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFJLYAC, mapping = aes(x= JobLevel, y=YearsAtCompany, color=PecentAttrit, size = totalPeople)) +
  geom_point() + scale_color_gradient(low = 'blue', high = 'red')

AFYCRYAC <- AF %>% group_by(YearsInCurrentRole, YearsAtCompany) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFYCRYAC, mapping = aes(x= YearsInCurrentRole, y=YearsAtCompany, color=PecentAttrit, size = totalPeople)) +
  geom_point() + scale_color_gradient(low = 'blue', high = 'red')

AFYSLPYAC <- AF %>% group_by(YearsSinceLastPromotion, YearsAtCompany) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFYSLPYAC, mapping = aes(x= YearsSinceLastPromotion, y=YearsAtCompany, color=PecentAttrit, size = totalPeople)) +
  geom_point() + scale_color_gradient(low = 'blue', high = 'red')

AFYSLPYAC <- AF %>% group_by(MonthlyIncome, MonthlyRate) %>% summarise(PecentAttrit = sum(AtrY)/n(), totalPeople = n())
ggplot(data = AFYSLPYAC, mapping = aes(x= YearsSinceLastPromotion, y=YearsAtCompany, color=PecentAttrit, size = totalPeople)) +
  geom_point() + scale_color_gradient(low = 'blue', high = 'red')
