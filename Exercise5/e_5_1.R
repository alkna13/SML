source("C:/Users/Alec/OneDrive/SML/exercise 1/loadImage.R")
library(caret) ## needed for confusion Matrix of RSNNS
library(Rcpp)
library(RSNNS)
library(kernlab)



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
x_l = loadMultiplePersonsData(300, 0, 0, "C:/Users/Alec/OneDrive/SML/SVN/2017/group")#loading group 3-8 takes ca. 30 min

#shuffle data
set.seed(990)
dataset_shuffle <- x_l[sample(nrow(x_l)),]
#will be split further below

dataset_decoded_targets <- decodeClassLabels(dataset_shuffle[,1]) # built in function of RSNNS
#remove the classifier from dataset
dataset_values = dataset_shuffle[,2:dim(dataset_shuffle)[2]]

rm(dataset_shuffle)
#ratio of 0.2 would mean 20% test set & 80% training
dataset_svm <- splitForTrainingAndTest(dataset_values, dataset_decoded_targets, ratio=0.2) 
dataset_svm <- normTrainingAndTestSet(dataset_svm, type="0_1")


result<- ksvm(x=dataset_svm$inputsTrain,y=dataset_svm$targetsTrain,kernel="rbfdot",kpar="automatic",C=1,type="C-svc")

output<- predict(result, dataset_svm$inputsTest, type = "response")






