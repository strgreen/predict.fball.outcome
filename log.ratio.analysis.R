#Load in libaries

library(lme4)
library(lmerTest)
library(cvTools)
library(jpeg)
library(spatstat)
library(reshape2)
library(rms) #For r2 measures of logistic regression

#Clear Workspace
rm(list=ls())

#load in custom local functions
source("C:/Users/Steve Green/Google Drive/GreenAnalytics/predict.fball.outcome/local.functions.R")

#Set out path
out.path = "C:/Users/Steve Green/Google Drive/GreenAnalytics/predict.fball.outcome/"


#Load in the data
raw.dataTable = read.csv("C:/Users/Baruch Spinoza/Google Drive/GreenAnalytics/predict.fball.outcome/process.data.no.ratio.csv")
raw.dataTable = read.csv("C:/Users/Steve Green/Google Drive/GreenAnalytics/predict.fball.outcome/process.data.no.ratio.csv")


#Only inlcude quantative varaibles
data.Table = raw.dataTable[,7:26]
#Add the outcome variable for testing
data.Table$Outcome = raw.dataTable$Outcome
#remove inf value
data.Table[is.infinite(as.matrix(data.Table))]=0
data.Table = na.omit(data.Table)

full.model = lrm(Outcome ~ ., data=data.Table )

resolution = 25

#find mean for each column
mean.table = colMeans(as.matrix(data.Table))
   
#intiliaze array
design = matrix(nrow = resolution * resolution, ncol = dim(as.matrix(mean.table))[1])

#build a design with mean for each measure
for (index in seq(1,dim(as.matrix(mean.table))[1]) ) {
  design[,index] = rep(mean.table[index],resolution*resolution)
}

#label columns of design
colnames(design) =  names(data.Table)
#turn design into a data frame
design = as.data.frame(design)

predict.cube = create.predict.cube(data.Table,resolution)

#compute mean across each slice
mean.cube = matrix(nrow = resolution, ncol = resolution)

for (row in seq(1,dim(predict.cube)[1])) {
  for (col in seq(1,dim(predict.cube)[2])) {
   
    mean.cube[row,col] = mean(predict.cube[row,col,])
    
  }
}

####################################################################
#produce some figures to view the maps with

par(mfrow=c(3,3))

for (slice in seq(1,9)) {
  image(predict.cube[,,slice], col = heat.colors(50,alpha=1))
}

par(mfrow=c(1,1))

image(blur(as.im(mean.cube),sigma=.25,bleed = FALSE,normalise = TRUE),col=heat.colors(50,alpha=1))  

pre.image =blur(as.im(predict.matrix),sigma=1,bleed = FALSE, normalize = TRUE)
image(predict.cube[,,9], col = heat.colors(50,alpha=1))




#############################
#produce a csv file

#Order of maps
#------------------

#10-Average Rush.Yards
#9-Rush.Yards
#8-Rush.Attempts
#7-Average Pass yard
#6-Pass Percent Complete
#5-Pass.Attempts
#4-Fumbles
#3-Interceptions    
#2-1st.downs              
#1-yards 


#0-averaging all maps together

#attach mean slice to predict cube
out.array = array(dim=c(resolution,resolution,11))
out.array[,,1] = mean.cube
out.array[,,2:11] = predict.cube

write.csv(melt(out.array),paste(out.path,"log.predict.csv", sep = ""), row.names =FALSE)

#create a cube with estimaged values
est.cubes = create.est.cubes(data.Table,resolution)

#write out cubes containing estimates 
write.csv(melt(est.cubes$Home.Values),paste(out.path,"log.predict.home.values.csv", sep = ""), row.names =FALSE)
write.csv(melt(est.cubes$Visitor.Values),paste(out.path,"log.predict.vist.values.csv", sep = ""), row.names =FALSE)



########################################################################
#DONT USE: Produces a  montonic result
########################################################################

#produce an probablilty map using estimated values for each measure

#Create an empty array to contain all esitmated meaures
est.design = array( dim= c(resolution*resolution, dim(data.Table)[2] -1 ))

#loop through each using min max and resolution to produce a design matrix
for (index in seq(1, dim(data.Table)[2] -1 )) {
est.design[,index] = seq(min(data.Table[,index]),max(data.Table[,index]), length.out = resolution*resolution)
}

#fit model
model.predict = predict(full.model,data=est.design,type="fitted")

#reshape into 25 x 25
model.predict = matrix(model.predict,nrow=resolution,ncol=resolution)

temp = model.predict

image(model.predict, col = heat.colors(50,alpha=1))

