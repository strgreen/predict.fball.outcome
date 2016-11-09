#Load in libaries

library(lme4)
library(lmerTest)
library(cvTools)
library(jpeg)
library(ggplot2)
library(spatstat)
library(reshape2)
library(rms) #For r2 measures of logistic regression
library(leaps) #Find best model using RSS (using function regsubsets)
library(glmnet) #Lass and Ridge Regression

#Clear Workspace
rm(list=ls())

#Load in the data
raw.dataTable = read.csv("C:/Users/Baruch Spinoza/Google Drive/GreenAnalytics/predict.fball.outcome/process.data.no.ratio.csv")
raw.dataTable = read.csv("C:/Users/Steve Green/Google Drive/GreenAnalytics/predict.fball.outcome/process.data.no.ratio.csv")


############################################
#test models
kFolds = 5

#Initialize sen and spec arrays
sens.array = 1
spec.array = 1
rsqr.array = 1

sqr = 1
sen = 1
spe = 1
  #Predict outcomes on test data

###########################
#####Test Full Model#######
###########################

#Only inlcude quantative varaibles
data.Table = raw.dataTable[,7:26]
#Add the outcome variable for testing
data.Table$Outcome = raw.dataTable$Outcome
#remove inf value
data.Table[is.infinite(as.matrix(data.Table))]=0
data.Table = na.omit(data.Table)

#CrossValidation


#Run the model 500 times to esimate sensitivity and specficity
for (index in seq(1,500)){
  bins = cvFolds(dim(data.Table), kFolds)
  train.data = data.Table[bins$subsets[bins$which>1],]
  test.data = data.Table[bins$subsets[bins$which==1],]

  #Fit model to a binomial distribution (Logistic)
  full.model = lrm(Outcome ~ ., data=train.data )
  
  #Predict outcomes on test data
  pred.model = predict(full.model,test.data,type = "fitted")
  #Convert probabilties into binary ( 1 (win) or 0 (loss))
  results = as.numeric((pred.model>.5) + 0)
  
  true.positives = sum(test.data$Outcome==1 & results ==1)
  false.positives = sum(test.data$Outcome == 0 & results==1)
  false.negatives = sum(test.data$Outcome == 1 & results == 0 )
  true.negatives = sum(test.data$Outcome ==0 & results == 0)
  
  sen[index] = true.positives / (true.positives + false.negatives)
  spe[index] = true.negatives / (true.negatives + false.positives)
  sqr[index] = full.model$stats[10]
  
}

#Compute sensitivy and specifity of full model
sens.array[1] = mean(sen)
spec.array[1] = mean(spe)
rsqr.array[1] = mean(sqr)


    
#Run the model a thousand to esimate sensitivity and specficity
for (index in seq(1,500)){
  bins = cvFolds(dim(data.Table), kFolds)
  train.data = data.Table[bins$subsets[bins$which>1],]
  test.data = data.Table[bins$subsets[bins$which==1],]
  
  #Fit model to a binomial distribution (Logistic)
  full.model = lrm(Outcome ~ Home.Yards + Visitor.Yards + Home.1st.Downs + 
                     Vistior.1st.Downs + Home.Interceptions + Visitor.Interceptions +
                     Home.Pass.Attempts + Visitor.Pass.Attempts + Home.Percent.Complete + 
                     Visitor.Percent.Complete + Home.Average.Rush.Yards + 
                     Visitor.Average.Rush.Yards , 
                   data=train.data )
  
  pred.model = predict(full.model,test.data,type = "fitted")
  #Convert probabilties into binary ( 1 (win) or 0 (loss))
  results = as.numeric((pred.model>.5) + 0)
  
  true.positives = sum(test.data$Outcome==1 & results ==1)
  false.positives = sum(test.data$Outcome == 0 & results==1)
  false.negatives = sum(test.data$Outcome == 1 & results == 0 )
  true.negatives = sum(test.data$Outcome ==0 & results == 0)
  
  sen[index] = true.positives / (true.positives + false.negatives)
  spe[index] = true.negatives / (true.negatives + false.positives)
  sqr[index] = full.model$stats[10]
  
}

#Compute sensitivy and specifity of full model
sens.array[2] = mean(sen)
spec.array[2] = mean(spe)
rsqr.array[2] = mean(sqr)


###########################
#######Subset Model########
###########################

#Compute new varaibles reflected as ratios or differnces between teams
yard.ratio = raw.dataTable$Home.Yards/raw.dataTable$Visitor.Yards
frst.ratio = raw.dataTable$Home.1st.Downs/raw.dataTable$Vistior.1st.Downs

rush.yard.ratio = raw.dataTable$Home.Rush.Yards/raw.dataTable$Visitor.Rush.Yards
rush.atmp.ratio = raw.dataTable$Home.Rush.Attempts/raw.dataTable$Visitor.Rush.Attempts
rush.avge.ratio = raw.dataTable$Home.Average.Rush.Yards/raw.dataTable$Visitor.Average.Rush.Yards

pass.atmp.ratio = raw.dataTable$Home.Pass.Attempts/raw.dataTable$Visitor.Pass.Attempts
pass.prct.ratio = raw.dataTable$Home.Percent.Complete/raw.dataTable$Visitor.Percent.Complete
pass.avge.ratio = raw.dataTable$Home.Average.Pass.Yards/raw.dataTable$Visitor.Average.Pass.Yards

intr.contr = as.numeric(raw.dataTable$Home.Interceptions - raw.dataTable$Visitor.Interceptions)
fmbl.contr = as.numeric(raw.dataTable$Home.Fumble - raw.dataTable$Visitor.Fumble)

#form a new data frame composed of the above ratio measures
sub.dataTable = data.frame(yard.ratio, frst.ratio, rush.yard.ratio, rush.atmp.ratio, rush.avge.ratio,
                           pass.atmp.ratio, pass.prct.ratio, pass.avge.ratio,
                           intr.contr, fmbl.contr )

#Add the outcome variable for testing
sub.dataTable$Outcome = raw.dataTable$Outcome
#remove inf value by setting them to zero
sub.dataTable[is.infinite(as.matrix(sub.dataTable))]=0
#remove na responses
sub.dataTable = na.omit(sub.dataTable)


for (index in seq(1,500)){

  bins = cvFolds(dim(sub.dataTable), kFolds)
  train.data = sub.dataTable[bins$subsets[bins$which>1],]
  test.data = sub.dataTable[bins$subsets[bins$which==1],] 
  
  #Fit model to a binomial distribution (Logistic)
  ratio.model = lrm(Outcome ~ ., data=train.data)
 
  #Predict outcomes on test data
  pred.model = predict(ratio.model,test.data,type = "fitted")
  #Convert probabilties of prediction model to binary 
  results = as.numeric((pred.model>.5) + 0)
 
  true.positives = sum(test.data$Outcome==1 & results ==1)
  false.positives = sum(test.data$Outcome == 0 & results==1)
  false.negatives = sum(test.data$Outcome == 1 & results == 0 )
  true.negatives = sum(test.data$Outcome ==0 & results == 0)
  
  #compute sensitivty
  sen[index] = true.positives / (true.positives + false.negatives)
  #compute specificity
  spe[index] = true.negatives / (true.negatives + false.positives)
  #computer rsquare (explained variance)
  sqr[index] = ratio.model$stats[10]
  
}

#Compute mean sensitivy and specifity of full model
sens.array[2] = mean(sen)
spec.array[2] = mean(spe)
rsqr.array[2] = mean(sqr)

###########################
#######Subset Model########
###########################

#Based on the subset findings we might be able to get good prediction
#with just a few variables: pass.atmp.ratio, rush.avge.ration, 
#rush.atmp.ratio, intr.contr, fmbl.contr

for (index in seq(1, 500)){
  bins = cvFolds(dim(sub.dataTable), kFolds)
  train.data = sub.dataTable[bins$subsets[bins$which>1],]
  test.data = sub.dataTable[bins$subsets[bins$which==1],]
  
  sub.model = lrm(Outcome ~ yard.ratio + frst.ratio +  
                  rush.atmp.ratio + rush.avge.ratio  + 
                  pass.prct.ratio + 
                  intr.contr + fmbl.contr, 
                  data=train.data )
  
  pre.model = predict(sub.model,test.data,type = "fitted")
  results = as.numeric((pred.model>.5) + 0)
  
  true.positives = sum(test.data$Outcome==1 & results ==1)
  false.positives = sum(test.data$Outcome == 0 & results==1)
  false.negatives = sum(test.data$Outcome == 1 & results == 0 )
  true.negatives = sum(test.data$Outcome ==0 & results == 0)
  
  #compute sensitivty
  sen[index] = true.positives / (true.positives + false.negatives)
  #compute specificity
  spe[index] = true.negatives / (true.negatives + false.positives)
  #computer r square (explained variance)
  sqr[index] = sub.model$stats[10]
  
}

sens.array[3] = mean(sen)
spec.array[3] = mean(spe)
rsqr.array[3] = mean(sqr)


############################
#end test models




size_of_matrix = 15

#Produce a matrix using logistic regression

logit.model = glm(Outcome ~  Interception.Contrast * First.Down.Ratio,  
                  family = 'binomial', data = out.dataTable)

Interception.Contrast = seq(from = -8, to = 8, length.out =  size_of_matrix)
Interception.Contrast = rep(Interception.Contrast,size_of_matrix)
Interception.Index = seq(from = 1, to = size_of_matrix)
Intercpetion.Index = rep(Interception.Index,size_of_matrix)

First.Down.Index = seq(from =0 ,to = 2, length.out =size_of_matrix * size_of_matrix)
First.Down.Index = cut(First.Down.Ratio,size_of_matrix,labels = FALSE)
First.Down.Ratio = First.Down.Index/5

#Create a data frame to run the model with
pre.dataFrame = data.frame(Interception.Contrast,First.Down.Ratio)
#Run the logistic model 
pre.out = predict(logit.model, pre.dataFrame, type = "response")







#initialize an empty matrix
pre.matrix =matrix(nrow = size_of_matrix,ncol =size_of_matrix)
#Build a matrix
for( index in 1:length(pre.out))
{
  pre.matrix[First.Down.Index[index],Intercpetion.Index[index]] = pre.out[index] 
}

pre.image =blur(as.im(pre.matrix),sigma=1,bleed = FALSE, normalize = TRUE)
image(pre.image, col = heat.colors(50,alpha=1))



#computer mean of outcome
mn.outcome = mean(out.dataTable$Outcome)
#initialize an empty matrix
out.matrix =matrix(nrow = size_of_matrix,ncol =size_of_matrix)
#fill matrix with average outcome
out.matrix[,]=mn.outcome

#Create categories out of the varibles of interest
out.dataTable$fDown.cat= cut(out.dataTable$First.Down.Ratio,size_of_matrix, labels = FALSE)
out.dataTable$Inter.cat= cut(out.dataTable$Interception.Contrast,size_of_matrix, labels = FALSE)

#Compute mean table
tb.mn.outcome = aggregate(out.dataTable$Outcome, 
                          list(out.dataTable$fDown.cat,out.dataTable$Inter.cat),
                          FUN = "mean")

#Using tb.mn.outcome fill arrray and compute average for each cell
for( index in 1:dim(tb.mn.outcome)[1])
{
  out.matrix[tb.mn.outcome[index,1],tb.mn.outcome[index,2]] = tb.mn.outcome[index,3] 
}

out.image =blur(as.im(out.matrix),sigma=1,bleed = FALSE, normalize = FALSE)
image(out.image, col = heat.colors(50,alpha=1))

out.image =blur(as.im(out.matrix),sigma=1,bleed = FALSE, normalize = TRUE)
image(out.image, col = heat.colors(50,alpha=1))



ggplot(mn.outcome , aes(Group.1, Group.2))+
  geom_tile(data=mn.outcome, aes(fill=x), color="white")+
  scale_fill_gradient2(low="blue", high="Orange", mid="white", 
                       midpoint=.5, limit=c(0,1),name="Correlation\n(Pearson)")
  



#Plot the model the 



Yard.Ratio = seq(0,3.0,.125)
First.Down.Ratio = seq(0,3.0,.125)

pre.dataTable = data.frame(fdwn.val,yard.val)

par(mfrow=c(1,1))

estY = predict(logit.model,pre.dataTable,type="response")
plot(out.dataTable$Yard.Ratio,out.dataTable$Outcome)
lines(Yard.Ratio,estY)
lines(First.Down.Ratio,estY,col ="red")

