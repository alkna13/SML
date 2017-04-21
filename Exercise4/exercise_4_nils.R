source("A:/Machine_Learning/Basefolder/loadImage.R")
library(rpart)
library(rpart.plot)

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
# 0. Prerequisites:
# Load data and apply pca
#
########################################################



#select which you want. load only once. takes forever
#x = loadSinglePersonsData(300, 4, 3, "A:/Machine_Learning/2017/group")
#left to right the inputs are: (dpi,start_group,end_group,file_location)
x_l = loadMultiplePersonsData(300, 4, 4, "A:/Machine_Learning/2017/group")#loading group 3-8 takes ca. 30 min

person_dependent = TRUE
use_multi = TRUE
test_split = 0.5  #how large should the training set be 0.9=90/10 training/testing

set.seed(990)

if (use_multi)
{
  if (person_dependent)
  {
    dataset_shuffle <- x_l[sample(nrow(x_l)), ]
  } else
  {
    dataset_shuffle <- x_l
  }
} else
{
  dataset_shuffle <- x[sample(nrow(x)), ]
  print("no use_multi ")
}

#create the training set
dataset_train <-
  array(, dim = c((dim(dataset_shuffle)[1] * test_split), dim(dataset_shuffle)[2]))
for (i in 1:dim(dataset_train)[1])
{
  #training set
  dataset_train[i, ] <- dataset_shuffle[i, ]
}

#create the testing set
dataset_test <-
  array(, dim = c(dim = c((
    dim(dataset_shuffle)[1] - dim(dataset_train)[1]
  ), dim(dataset_shuffle)[2])))
for (i in 1:dim(dataset_test)[1])
{
  #kNN testing set
  dataset_test[i, ] <-
    dataset_shuffle[i + (dim(dataset_shuffle)[1] * test_split), ]
}

#training set classification vector
train_class <- array(, dim = c(1, dim(dataset_train)[1]))
for (i in 1:dim(dataset_train)[1])
{
  train_class[i] = dataset_train[i, 1]
}

#testing set classification vector
test_class <- array(, dim = c(1, dim(dataset_test)[1]))
for (i in 1:dim(dataset_test)[1])
{
  test_class[i] = dataset_test[i, 1]
}

print("Beginning pca on dataset")
#pca <- prcomp(dataset_shuffle)
rm(dataset_shuffle)


pca_time <- proc.time()

pca_train <- prcomp(dataset_train) 
#rotate test set the same way as the training set was rotated by PCA
pca_test<-dataset_test %*% pca_train$rotation

pca_time <- proc.time() - pca_time
#time is third variable

#Plot and resume results

#variance
plot(pca_train$sdev,
     main = "Variance by PCA",
     xlab = "PCA",
     ylab = "Variance")

#accumulated variance absolute
total_var = 0                                         #maximum variance counter variable
total_var_plot = array(, dim = c(1, length(pca_train$sdev)))    #array tracking accumulated total variance(absolute)
for (i in 1:length(pca_train$sdev))
{
  total_var <- total_var + pca_train$sdev[i]
  total_var_plot[i] <- total_var
}
plot(
  1:3365,
  total_var_plot,
  main = "Accumulated Variance (absolute)",
  xlab = "PC",
  ylab = "Acumulated Variance"
)


#accumulated variance percentage
acc_var = 0                                           #accumulated variance
acc_var_plot = array(, dim = c(1, length(pca_train$sdev)))      #array tracking accumulated variance(%)
for (i in 1:length(pca_train$sdev))
{
  acc_var <- acc_var + pca_train$sdev[i]
  acc_var_plot[i] <- acc_var / (total_var / 100)
}
plot(1:3365,
     acc_var_plot,
     main = "Accumulated Variance (%)",
     xlab = "PC",
     ylab = "Acumulated Variance")


#20 values output in array
acc_var_out <- array(0, dim = c(20))
for (i in 1:20)
{
  acc_var_out[i] <- acc_var_plot[i * length(acc_var_out / 20)]
}

########################################################
#
# 4.1.1 Decision point and information gain
# 5 graphs, one for each component, and the highest information gain
#
########################################################
### 0. Extract the first 5 PCAs for each cipher-image
rows = dim(dataset_train)[1]
firstFivePCAs <- array(0, dim = c(rows,6))
for (cipherIndex in 1:rows)
{
    firstFivePCAs[cipherIndex,1:5] <- as.numeric(pca_train$x[cipherIndex,(1:5)])
}
colnames(firstFivePCAs)<- c("PCA1","PCA2","PCA3","PCA4","PCA5","class")
#append class(cipher) to table
firstFivePCAs[,6] <- train_class 


### 1. calculate entropy before split
totalAmountOfCiphers = dim(firstFivePCAs)[1]

#calculate initial root entropy (value must be between 0 - log2(10)=3.322)
entropy = 0 
for(c in 0:9){
  # SUm of all values with this class(cipher) and this value
  sumThisCipher = sum(firstFivePCAs[,6] == c) 
  
  #calculate probability
  p <- sumThisCipher/totalAmountOfCiphers
  
  #set 0 if no occurence (log would give -inf)
  if(p!=0) temp = (p*log2(p))
  else temp = 0
  
  entropy = entropy + temp*-1
}



### 3. Beginn with Recursive Binary Splitting to calculate decision point using rpart (where to split?)
#TODO: Compute decision point for PCAs and associated information gain


########################################################
#
# 4.1.2 Compute and plot decision tree
#
########################################################
dataPCA <- as.data.frame(firstFivePCAs)
tree <- rpart(class ~ ., data = dataPCA, method = "class")
#plot tree
rpart.plot(tree, extra=1+100, nn=TRUE)

### 4. Calculate Entropies and Information gain


