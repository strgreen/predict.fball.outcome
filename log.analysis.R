#Load in libaries

library(lme4)
library(lmerTest)
library(cvTools)


#Clear Workspace
rm(list=ls())

#Load in the data
dataTable = read.csv("C:/Users/Baruch Spinoza/Google Drive/GreenAnalytics/predict.fball.outcome/process.data.csv")
dataTable = read.csv("C:/Users/Steve Green/Google Drive/GreenAnalytics/predict.fball.outcome/process.data.csv")


#Look at variable distributions
hist(dataTable$Yard.Ratio)
hist(dataTable$First.Down.Ratio)
hist(dataTable$Interception.Contrast)
hist(dataTable$Fumble.Contrast)

#Remove anythin over 3 (more less arbitraily choosen)
out.dataTable = dataTable[dataTable$First.Down.Ratio < 3,]

#rexamine variables after removing outliers
hist(out.dataTable$Yard.Ratio)
hist(out.dataTable$First.Down.Ratio)
hist(out.dataTable$Interception.Contrast)
hist(out.dataTable$Fumble.Contrast)

#Model with all data 
logit.model = glm(Outcome ~  Yard.Ratio + First.Down.Ratio + Interception.Contrast + Fumble.Contrast, 
                  family = 'binomial', data = out.dataTable)
summary(logit.model)

#CrossValidation

bins = cvFolds(dim(out.dataTable), 10)
train.data = out.dataTable[bins$subsets[bins$which>1],]
test.data = out.dataTable[bins$subsets[bins$which==1],]

logit.model = glm(Outcome ~  Yard.Ratio + First.Down.Ratio + Interception.Contrast + Fumble.Contrast, 
                  family = 'binomial', data = train.data)

pre.model = predict(logit.model,test.data,type = "response")
results = (pre.model>.5) + 0
accuracy = mean(results == test.data$Outcome)


