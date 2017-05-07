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
#will be split further below


########################################################
#
# 5.1.1 Formatting training classes  
# output needs to be a binary matrix not number values 
# for implementation see function "formatClassLabels" at the top
# 
########################################################

dataset_decoded_targets <- formatClassLabels(dataset_shuffle)

#remove the classifier from dataset
dataset_values = dataset_shuffle[,2:dim(dataset_shuffle)[2]]
#remove unnecessary shuffled dataset
rm(dataset_shuffle)
#Split training and testset with splitting function of RSNNS
dataset_mlp <- splitForTrainingAndTest(dataset_values, dataset_decoded_targets, ratio=test_split)
#Normalize data
dataset_mlp <- normTrainingAndTestSet(dataset_mlp, type="0_1")

########################################################
#
# 5.1.2 Train neural network
#  
########################################################
#train model with training values, targets to values, size of units, 
#the params for the learning functions and the maximum iterations
nnModel <- mlp(dataset_mlp$inputsTrain, dataset_mlp$targetsTrain, size=c(20,20,20), learnFuncParams = 0.1, maxit = 100 )
print(nnModel)

########################################################
#
# 5.1.3 Evaluate neuronal network with test data
#  
########################################################

#Evaluate the model with test data
nnModel <- mlp(dataset_mlp$inputsTrain, dataset_mlp$targetsTrain, size=c(20,20,20), learnFuncParams = 0.1, maxit = 100, 
               inputsTest = dataset_mlp$inputsTest, targetsTest = dataset_mlp$targetsTest)
predictions <-  predict(nnModel, dataset_mlp$inputsTest)

print(nnModel)

#Plot some graphs --> interpretation help: http://beyondvalence.blogspot.dk/2014/03/neural-network-prediction-of.html or https://www.google.dk/url?sa=t&rct=j&q=&esrc=s&source=web&cd=2&ved=0ahUKEwinvMbYrd7TAhWFlCwKHYHXDR4QFgg0MAE&url=https%3A%2F%2Fwww.jstatsoft.org%2Farticle%2Fview%2Fv046i07%2Fv46i07.pdf&usg=AFQjCNHVRgzP389c8ReCHPmS9bd_qYm0Ow&sig2=zUdvGi8oo6CnlgoctJrYqw&cad=rja
plotIterativeError(nnModel, main="Iterative Error")# training is black line, test is red line
legend("topright", c("training data", "test data"), col=c("black","red"), lwd=c(1,1))

plotRegressionError(predictions[,2], dataset_mlp$targetsTest[,2], main="Regression Error")
legend("bottomright", c("optimal", "linear fit"), col=c("black","red"), lwd=c(1,1))

plotROC(fitted.values(nnModel)[,2], dataset_mlp$targetsTrain[,2]) 
plotROC(predictions[,2], dataset_mlp$targetsTest[,2])



########################################################
#
# 5.1.4 Experiment with various parameters
#  
########################################################

##a) Hidden Structure
#a1) Layers and nodes
#Layers:
# ?
#Nodes:
#TODO: CHanging size value


#a2) Internal Learning parameters (Learning rate and maximum output difference)
# Learning rate

#Maximum output difference