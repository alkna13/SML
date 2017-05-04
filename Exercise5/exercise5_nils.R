################################################################
#
# source and load dependencies
#
################################################################
source("A:/Machine_Learning/Basefolder/loadImage.R")
library(RSNNS)


################################################################
#
# Function to load multiple persons data
#
################################################################

loadMultiplePersonsData <-
  function(dpi = 300,
           startgrp = 4,
           endgrp = 4,
           location)
  {
    options(show.error.messages = FALSE)
    memstart = 0
    x = try(loadSinglePersonsData(dpi, startgrp, 0, location))
    #DPI change
    if (class(x) == "try-error")
    {
      x = loadSinglePersonsData(dpi, startgrp, 1, location)#DPI change
      memstart = 1
    }
    
    for (i in startgrp:endgrp)
    {
      for (j in 0:4)
      {
        if (j != memstart || i != startgrp)
        {
          y = try(loadSinglePersonsData(dpi, i, j, location))
          #DPI change
          if (class(y) != "try-error" && y[1, 1] == 0)
          {
            x <- rbind2(y, x)
          }
        }
      }
    }
    options(show.error.messages = TRUE)
    return(x)
  }

########################################################
#
# Function for 5.1.1 Formatting training classes  
# output needs to be a binary matrix not number values
# 
########################################################

formatClassLabels <- function(dataset){
  id <- as.data.frame(dataset)
  lev <- sort(unique(id$V1)) #get all different unique class values (0-9)
  nnDecodedClass <- matrix(nrow = length(id$V1), ncol = 10, data = 0)
  #check each row, with each output (columns 0-9) --> if equals: 1 if not: 0
  for(i in 1:length(id$V1)) {
    matchList <- match(lev,toString(as.integer(id$V1[i])))
    matchList[is.na(matchList)] <- 0
    nnDecodedClass[i,] <- matchList
  } 
  decodedClass <- as.data.frame(nnDecodedClass)
  #name columns with cipher names
  colnames(decodedClass)<-c("0","1","2","3","4","5","6","7","8","9")
  
  return(decodedClass)
}


########################################################
#
# 0. Prerequisites
# Obtaining data
# 
########################################################
#left to right the inputs are: (dpi,start_group,end_group,file_location)
x_l = loadMultiplePersonsData(300, 4, 4, "A:/Machine_Learning/2017/group")#loading group 3-8 takes ca. 30 min
test_split = 0.5 #how large should the training set be 0.9=90/10 training/testing


#shuffle data
set.seed(990)
dataset_shuffle <- x_l[sample(nrow(x_l)),]

#create the training set
dataset_train<- array(, dim=c((dim(dataset_shuffle)[1]*test_split),dim(dataset_shuffle)[2]))
for(i in 1:dim(dataset_train)[1])
{
  #training set
  dataset_train[i,]<-dataset_shuffle[i,]
}

#create the testing set
dataset_test<- array(, dim=c(dim=c((dim(dataset_shuffle)[1]-dim(dataset_train)[1]),dim(dataset_shuffle)[2])))
for(i in 1:dim(dataset_test)[1])
{
  #testing set
  dataset_test[i,]<-dataset_shuffle[i+(dim(dataset_shuffle)[1]*test_split),]
}


########################################################
#
# 5.1.1 Formatting training classes  
# output needs to be a binary matrix not number values 
# for implementation see function "formatClassLabels" at the top
# 
########################################################

trainingClass <- formatClassLabels(dataset_train)

#remove the classifier from training set
dataset_train_values = dataset_train[,2:dim(dataset_train)[2]]

########################################################
#
# 5.1.2 Train neural network
#  
########################################################
#train model with training values, targets to values, size of units, 
#the params for the learning functions and the maximum iterations
nnModel <- mlp(dataset_train_values, trainingClass, size=c(20,20,20), learnFuncParams = 0.1, maxit = 100 )
print(nnModel)

########################################################
#
# 5.1.3 Evaluate neuronal network with test data
#  
########################################################
#format the outputs for test data
testClass <- formatClassLabels(dataset_test)

#remove the classifier from test set
dataset_test_values = dataset_test[,2:dim(dataset_test)[2]]

#Evaluate the model with test data
nnModel <- mlp(dataset_train_values, trainingClass, size=c(20,20,20), learnFuncParams = 0.1, maxit = 100, 
               inputsTest = dataset_test_values, targetsTest = testClass)
predictions <-  predict(nnModel, dataset_test_values)

print(nnModel)

#Plot some graphs
plotIterativeError(nnModel)# training is black line, test is red line
plotRegressionError(predictions[,2], testClass[,2], pch = 3) 
plotROC(fitted.values(nnModel)[,2], trainingClass[,2]) 
plotROC(predictions[,2], testClass[,2])


