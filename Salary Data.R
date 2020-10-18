#Prepare a classification model using Naive Bayes for salary data 

#Data Description:
  
#  age -- age of a person
#workclass	-- A work class is a grouping of work 
#education	-- Education of an individuals	
#maritalstatus -- Marital status of an individulas	
#occupation	 -- occupation of an individuals
#relationship -- 	
#  race --  Race of an Individual
#sex --  Gender of an Individual
#capitalgain --  profit received from the sale of an investment	
#capitalloss	-- A decrease in the value of a capital asset
#hoursperweek -- number of hours work per week	
#native -- Native of an individual
#Salary -- salary of an individual

install.packages("readr")
library(readr)
setwd("C://Users//Lenovo//Desktop//ExcelR//Assignments//Naive Bayes")
getwd()

#Training and Test Data
SalTrain<-read.csv("SalaryData_Train.csv")
View(SalTrain)
SalTest<-read.csv("SalaryData_Test.csv")
View(SalTest)

#create a model of naive bayes
install.packages(e1071)
library(e1071)
model <- naiveBayes(factor(SalTrain$sex)~factor(age)+factor(workclass)+factor(educationno)+factor(maritalstatus)+factor(occupation)+factor(relationship)+factor(race)
        +factor(capitalgain)+factor(capitalloss)+factor(hoursperweek)+factor(native)+factor(Salary), data=SalTrain)
pred <- predict(model,newdata = SalTest[,-1])
mean(pred==SalTest[,1])


##going to do bagging methond'

acc<-NULL

for (i in 1:10) {
  SalTrain<-read.csv("SalaryData_Train.csv")
  SalTest<-read.csv("SalaryData_Test.csv")
  model <- naiveBayes(factor(SalTrain$sex)~factor(age)+factor(workclass)+factor(educationno)+factor(maritalstatus)+factor(occupation)+factor(relationship)+factor(race)
                                +factor(capitalgain)+factor(capitalloss)+factor(hoursperweek)+factor(native)+factor(Salary), data=SalTrain)
  pred <- predict(model,newdata = SalTest[,-1])
  acc <- c(acc,mean(pred==SalTest[,1]))
}

mean(acc)
