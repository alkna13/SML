################################################################
#
# source and load dependencies
#
################################################################
source("A:/Machine_Learning/Basefolder/loadImage.R")
library(caret) ## needed for confusion Matrix of RSNNS
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
x_l = loadMultiplePersonsData(300, 0, 0, "A:/Machine_Learning/2017/group")#loading group 3-8 takes ca. 30 min

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

##
#dataset_decoded_targets <- formatClassLabels(dataset_shuffle[,1]) #decomment to use custom function
dataset_decoded_targets <- decodeClassLabels(dataset_shuffle[,1]) # built in function of RSNNS


#remove the classifier from dataset
dataset_values = dataset_shuffle[,2:dim(dataset_shuffle)[2]]
#remove unnecessary shuffled dataset
rm(dataset_shuffle)
#Split training and testset with splitting function of RSNNS
dataset_mlp <- splitForTrainingAndTest(dataset_values, dataset_decoded_targets, ratio=0.5) #ratio of 0.2 would mean 20% test set & 80% training
#Normalize data
dataset_mlp <- normTrainingAndTestSet(dataset_mlp, type="0_1")

########################################################
#
# 5.1.2 Train neural network
#  
########################################################
#train model with training values, targets to values, size of units, 
#the params for the learning functions and the maximum iterations
start.time <- proc.time()
nnModel <- mlp(dataset_mlp$inputsTrain, dataset_mlp$targetsTrain, size=c(20,20,20), learnFuncParams = 0.1, maxit = 100 )
proc.time() - start.time
print(nnModel)

########################################################
#
# 5.1.3 Evaluate neuronal network with test data
#  
########################################################
#Evaluate the model with test data
start.time <-proc.time()
nnModel <- mlp(dataset_mlp$inputsTrain, dataset_mlp$targetsTrain, size=10, learnFuncParams = c(0.1,0.9), maxit = 100, 
               inputsTest = dataset_mlp$inputsTest, targetsTest = dataset_mlp$targetsTest)
mlp_time <- proc.time() - start.time ##result will be time for training AND test. Subtract elapsed training time from before for test time

###### Starting Evaluations
## 1. Get targets and fitted values for calculating mean

#Select the highest value (guessed cipher) for each binary decoded row of target inputs
dataset_class_test <- (0:9)[apply(dataset_mlp$targetsTest,1,which.max)]
dataset_class_train <- (0:9)[apply(dataset_mlp$targetsTrain,1,which.max)]

#Select the highest value (guessed cipher) for each binary decoded row of fitted test values
nnModel_class_test <- (0:9)[apply(nnModel$fittedTestValues,1,which.max)]
nnModel_class_train <- (0:9)[apply(nnModel$fitted.values,1,which.max)]

#calculate accuracy
meanTest <- mean(dataset_class_test == nnModel_class_test)
meanTest
meanTrain <- mean(dataset_class_train == nnModel_class_train)
meanTrain
text <- paste("Time: ",mlp_time[3],  " | Acc Test: ",meanTest," | Acc Train", meanTrain)
write(text,file="plots/accuracys_time.txt")
## 2. Confusion matrix with fitted values vs targets
# First bring them to the same length (if some numbers never occured in prediction)
#test set
u = union(nnModel_class_test, dataset_class_test)
u <- sort.int(u)
t = table(factor(nnModel_class_test, u), factor(dataset_class_test, u))
test.con <- confusionMatrix(t)
test.con
write.csv2(test.con$table,file = "plots/confMatrix_testData.csv")
#train set

u = union(nnModel_class_train, dataset_class_train)
u <- sort.int(u)
t = table(factor(nnModel_class_train, u), factor(dataset_class_train, u))
train.con <- confusionMatrix(t)
train.con
write.csv2(train.con$table,file = "plots/confMatrix_trainData.csv")

## Plot some graphs --> interpretation help: http://beyondvalence.blogspot.dk/2014/03/neural-network-prediction-of.html or https://www.google.dk/url?sa=t&rct=j&q=&esrc=s&source=web&cd=2&ved=0ahUKEwinvMbYrd7TAhWFlCwKHYHXDR4QFgg0MAE&url=https%3A%2F%2Fwww.jstatsoft.org%2Farticle%2Fview%2Fv046i07%2Fv46i07.pdf&usg=AFQjCNHVRgzP389c8ReCHPmS9bd_qYm0Ow&sig2=zUdvGi8oo6CnlgoctJrYqw&cad=rja
## 3. Iterative Error graph
plotIterativeError(nnModel, main="Iterative Error")# training is black line, test is red line
legend("topright", c("training data", "test data"), col=c("black","red"), lwd=c(1,1))
#saving image
dev.copy(png,filename="plots/iterativeError.png");
dev.off ();

## 4. Plot Regression error and ROC for each cipher 
for(i in 1:10){
  
  #Regression error
  plotRegressionError(dataset_mlp$targetsTest[,i], nnModel$fittedTestValues[,i], main="Regression Error", sub=paste("Cipher: ",i-1))
  legend("bottomright", c("optimal", "linear fit"), col=c("black","red"), lwd=c(1,1))
  #saving image
  dev.copy(png,filename=paste("plots/regressionError_",i-1,".png"));
  dev.off ();
  
  #receiver Operating Characteristics (ROC) for each cipher
  
  #train set
  plotROC(nnModel$fitted.values[,i],dataset_mlp$targetsTrain[,i], main="ROC curve, training set", sub=paste("Cipher: ",i-1)) 
  #saving image
  dev.copy(png,filename=paste("plots/ROC_train_",i-1,".png"));
  dev.off ();
  
  #test set
  plotROC(nnModel$fittedTestValues[,i],dataset_mlp$targetsTest[,i], main="ROC curve, test set", sub=paste("Cipher: ",i-1))
  #saving image
  dev.copy(png,filename=paste("plots/ROC_test_",i-1,".png"));
  dev.off ();
}


########################################################
#
# 5.1.4 Experiment with various parameters
#  
########################################################
#Manipulate parameters before (ex 5.1.3)


##a) Hidden Structure
#a1) Layers and nodes
# change: size = c(20,20,20)

#a2) Internal Learning parameters (Learning rate and maximum output difference)
# Learning rate
#Maximum output difference