#Load in libaries

library(reshape2)
library(rms) #For r2 measures of logistic regression
library(leaps) #Find best model using RSS (using function regsubsets)
library(glmnet) #Lass and Ridge Regression

#Clear Workspace
rm(list=ls())

#Load in the data
raw.dataTable = read.csv("C:/Users/Baruch Spinoza/Google Drive/GreenAnalytics/predict.fball.outcome/process.data.no.ratio.csv")
raw.dataTable = read.csv("C:/Users/Steve Green/Google Drive/GreenAnalytics/predict.fball.outcome/process.data.no.ratio.csv")

#Set out path
out.path = "C:/Users/Steve Green/Google Drive/GreenAnalytics/predict.fball.outcome/"

###########################
#Full Model Reduction######
###########################

#Only inlcude quantative varaibles
data.Table = raw.dataTable[,7:26]
#Add the outcome variable for testing
data.Table$Outcome = raw.dataTable$Outcome
#remove inf value
data.Table[is.infinite(as.matrix(data.Table))]=0
data.Table = na.omit(data.Table)

#################
#Reducing variables based on model fit measures like RSS and Adjusted r2

  #Separate into test and train data for cross valdiation purposes
  bins = cvFolds(dim(data.Table), 10)
  train.data = data.Table[bins$subsets[bins$which>1],]
  test.data = data.Table[bins$subsets[bins$which==1],]

  ###################################################################
  #Iterating through all possible models and using rss to test model
    regfit.full=regsubsets(Outcome~.,data = train.data, nvmax = dim(train.data)[2])
    sum.regfit.full = summary(regfit.full)
    
    plot(sum.regfit.full$adjr2)
    plot(sum.regfit.full$rss)

    #Based on analysis a 12 term model is likely the best
    coef(regfit.full,12)
    
    ####################
    #Create a csv file containing the terms to keep for each model
    
      #Create an array listing all variables for each model
    
      colLabels = c("Home.Pass.Attempts", "Home.Percent.Complete", "Home.Average.Pass.Yards",
                  "Home.Rush.Attempts", "Home.Rush.Yards", "Home.Average.Rush.Yards",
                  "Home.Fumble", "Home.Interceptions", "Home.Yards", "Home.1st.Downs",
                  "Visitor.Pass.Attempts", "Visitor.Percent.Complete", "Visitor.Average.Pass.Yards",
                  "Visitor.Rush.Attempts", "Visitor.Rush.Yards", "Visitor.Average.Rush.Yards",
                  "Visitor.Fumble", "Visitor.Interceptions", "Visitor.Yards", "Vistior.1st.Downs")
    
      selIndex = matrix(nrow=sum(seq(1,20)),ncol=2)
      row = 0
      for (terms in seq(1,20)){
        selVars = as.array(rownames(melt(coef(regfit.full,terms))))

        for (selVar in seq(2,dim(selVars)) ) {

        row = row + 1
        print(row)
        
        selIndex[row,1] = terms 
        selIndex[row,2] = which(colLabels == selVars[selVar]) - 1          
        
        }
      }
    
      write.csv(selIndex,paste(out.path,"sel.var.csv", sep = ""), row.names =FALSE)

    
    
###########################
  #Forward Stepwise Selection
    regfit.fwd = regsubsets(Outcome ~ ., data = train.data, nvmax = dim(train.data)[2], method = "forward")
    sum.regfit.fwd = summary(regfit.fwd)
  
  plot(sum.regfit.fwd$adjr2)

  plot(sum.regfit.fwd$adjr2)
  plot(sum.regfit.full$rss)
  
#Based on analysis a 10 term model is likely the best
coef(regfit.fwd,10)

#Ridge Regression
#Convert data frame into  a matrix
x = model.matrix(Outcome~. ,train.data)[,-1]
y = train.data$Outcome

#Setup lambda values
grid = 10^seq(10,-2,length=100)

#Perfom regression (Alpha determines ridge(0) or lasso(1))
ridge.mod =glmnet(x,y, alpha=0,lambda =grid)

#Lasso Regression

#Convert data frame into  a matrix
xTrain = model.matrix(Outcome ~ . ,train.data)[,-1]
yTrain = train.data$Outcome

xTest = model.matrix(Outcome ~ . , test.data)[,-1]
yTest = train.data$Outcome

#Setup lambda values
grid = 10^seq(10,-2,length=100)

#Perfom regression (Alpha determines ridge(0) or lasso(1))
lasso.mod =glmnet(xTrain,yTrain, alpha=1,lambda = grid)

#Use cross validation to find the optimal lambda
cv.out = cv.glmnet(xTrain,yTrain,alpha=1)

#Find best lambda according to MSE
best.lambda = cv.out$lambda.min

#Refit the data using optimal lambda
lasso.mod = glmnet(x,y,alpha=1, lambda = best.lambda)

#Show coeffiecients of model with optimal lambda
predict(lasso.mod,type="coefficients",s=best.lambda,xTest)

