source("A:/Machine_Learning/Basefolder/loadImage.R")
library(rpart)
library(rpart.plot)
library(rattle) # ATTENTION: for this library, you need to install gtk+ first --> install.packages("RGtk2", depen=T, type="source")

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

calcEntropy <- function(data){
  
  if(is.na(data)[1]) return(log(10))
  ### 1. calculate entropy before split

  totalAmountOfCiphers = dim(data)[1]
  
  #calculate initial root entropy (value must be between 0 - log2(10)=3.322)
  entropy = 0
  
  for(c in 0:9){
    # SUm of all values with this class(cipher) and this value

    sumThisCipher = sum(data[,6] == c) 

    #calculate probability
    p <- sumThisCipher/totalAmountOfCiphers
  
    #set 0 if no occurence (log would give -inf)
    if(p!=0) temp = (p*log2(p))*-1
    else temp = 0
  
    entropy = entropy + temp
  }
  return(entropy)
}

########################################################
#
# 0. Prerequisites:
# Load data and apply pca
#
########################################################

#select which you want. load only once. takes forever
#x = loadSinglePersonsData(300, 4, 3, "A:/Machine_Learning/2017/group")
#left to right the inputs are: (dpi,start_group,end_group,file_location)
x_l = loadMultiplePersonsData(300, 4, 4, "A:/Machine_Learning/2017/group")#loading group 3-8 takes ca. 30 min

#shuffle data
set.seed(990)
dataset_shuffle <- x_l[sample(nrow(x_l)),]

print("Beginning pca on dataset") 
#leave out first column --> class identifier
pca <- prcomp(dataset_shuffle[,2:dim(dataset_shuffle)[2]])

########################################################
#
# 4.1.1 Decision point and information gain
# 5 graphs, one for each component, and the highest information gain
#
########################################################
### 0. Extract the first 5 PCAs for each cipher-image
rows = dim(dataset_shuffle)[1]
firstFivePCAs <- array(0, dim = c(rows,6))
for (cipherIndex in 1:rows)
{
    firstFivePCAs[cipherIndex,1:5] <- as.numeric(pca$x[cipherIndex,(1:5)])
}
colnames(firstFivePCAs)<- c("PCA1","PCA2","PCA3","PCA4","PCA5","class")
#append class(cipher) to table
firstFivePCAs[,6] <- dataset_shuffle[,1] 

### 1. Beginn with Recursive Binary Splitting to calculate decision point(where to split first root?)
#Define root, calculate entropy (entropy before) and intialize array for all iterations
root = firstFivePCAs
entropyBefore = calcEntropy(root)
rows= dim(root)[1]
iterations <-array()
#for each PCA, calculate 10 thresholds between min and max value
for(i in 1:5){
  min = min(root[,i])
  max = max(root[,i])
  range = max-min
  inc = range/10
  threshold = min + inc
  
  #calculate information gain for each threshold value (added inc/2 or else last threshold will be bigger as max)
  while(threshold <= (max+inc/2)){
    leftArray <- array()
    rightArray <- array()
    
    for(row in 1:rows){
      
      if(root[row,i] < threshold){
        leftArray = rbind.data.frame(leftArray,root[row,])
      } 
      else{
        rightArray = rbind.data.frame(rightArray,root[row,])
      }
    }
    #remove first (NA) row of each array, if there is more than one entry
    if(dim(leftArray)[1]>1) leftArray = leftArray[-1,]
    if(dim(rightArray)[1]>1) rightArray = rightArray[-1,]
    #calculate entropies and information gain
    entropyLeftAfter = calcEntropy(leftArray)
    entropyRightAfter = calcEntropy(rightArray)
    entropyAfter = (dim(leftArray)[1]/rows*entropyLeftAfter) +  (dim(rightArray)[1]/rows*entropyRightAfter)
    infoGain = entropyBefore - entropyAfter
    
    #save each iteration with PCA#, threshold, entropies and information gain
    iteration=c(i,threshold,entropyLeftAfter,entropyRightAfter,entropyAfter, infoGain)
    iterations <- rbind(iterations,iteration)
    #increase threshold by inc value
    threshold = threshold + inc
    
  }
  #remove first rows(NA) of array after the first iteration
  if(i==1)iterations=iterations[-1,]
  #plot the information gain for each threshold of this PCA#
  x<- iterations[(((i-1)*10)+1):(10*i),2]
  y<- iterations[(((i-1)*10)+1):(10*i),6]
  plot(x,y, main=paste("PCA ",i),sub="Data=Group4 (3 Person, whole data)", 
       xlab = paste("Threshold"), ylab="Information Gain")
}

#give the iterations table column names
colnames(iterations) <- c("PCA#","Threshold","entropyLeft","entropyRight","entropyTotal","informationGain")



########################################################
#
# 4.1.2 Compute and plot decision tree
#
########################################################
dataPCA <- as.data.frame(firstFivePCAs)
tree <- rpart(class ~ ., data = dataPCA, method = "class", control=rpart.control(cp=0.005))
#plot tree
rpart.plot(tree, extra=1+100, nn=TRUE)


########################################################
#
# 4.1.3 Cross validation
#
########################################################
### Show the cp values and errors for each decision and plot it
printcp(tree)
plotcp(tree)


