source("/home/nils/Rfiles/loadImage.R")
source("A:/Machine_Learning/Basefolder/loadImage.R")

#Libraries for plotting clusters
library(cluster)
library(fpc)


################################################################
#
# Exercise 3.2.1
#
# Dendogram with 5 instances of each digit (on one person)
#
################################################################

####### 1. Obtain data #######

# Load one person data
#x1 = loadSinglePersonsData(300,4,0,"/home/nils/2017/group") #0=Alec (amazon server path)
x1 = loadSinglePersonsData(300, 4, 0, "A:/Machine_Learning/2017/group") #0=Alec (local PC path)
#array for the cipher instances
cipher_array <- array(0, dim = c(50, dim(x1)[2]))

####### 2. Get five instances of each cipher out of training set #######
for (cipher_iter in 1:10)
{
  cipher = cipher_iter - 1
  
  for (i in 1:5) {
    row = (cipher * 5) + i
    row_train_set = (cipher * 400) + i
    
    cipher_array[row, ] <- x1[row_train_set, ]
    #cat("Row: ",row, " values= ", cipher_array[row,1:100],"\n")
    
  }
}
####### 2. Calculate distances and draw dendogram #######
dist_ciphers <-
  dist(cipher_array[, 2:dim(x1)[2]],  method = "euclidean") # calculate distances
hc_ciphers <- hclust(dist_ciphers)
plot(hc_ciphers,
     hang = -1,
     cex = 0.6,
     labels = cipher_array[, 1])

################################################################
#
# Exercise 3.2.2
#
# Dendogram with 5 instances of each digit with kmeans (on two person)
#
################################################################

####### 1. Obtain data for second person #######
x2 = loadSinglePersonsData(300, 4, 2, "A:/Machine_Learning/2017/group") #0=Louis (local PC path)
x_2p <- rbind2(x1, x2)
#order the array so the ciphers are in correct order from 0-9
x_2p <- x_2p[order(x_2p[, 1]), ]

#Step for each new cipher
step = 800 # 2* 400 numbers for each person


####### 2. Get 5 cluster out of each cipher #######
clusterSize = 5 #amount of clusters

cipher_cluster <- c()
label_cluster <- c()
for (i in 1:10)
{
  cipher = i - 1
  rowMin = cipher * step
  rowMax =  rowMin + step
  
  train_set <- x_2p[rowMin:rowMax, ]
  
  #training data for cipher i (remove class-identifier in first col)
  train_data_i <- train_set[train_set[, 1] == cipher,-1]
  #calculate kmeans with clusterSize clusters
  clusterData <- kmeans(train_data_i, clusterSize)
  
  
  #add center and label of the ith cipher to array
  cipher_cluster[[cipher + 1]] <- clusterData
  label_cluster[[cipher + 1]] <- c(1:clusterSize) * 0 + cipher
  # uncomment the following line to plot the cluster
  #plotcluster(train_data_i, clusterData$cluster)
  
  
}

rm(train_data_i)

#get training-label and data for knn-algorithm out of clustered data
train_lab <- factor(unlist(label_cluster))
train_dat <- array(0,dim= c(0,3364))
for (i in 1:10) {
  for(c in 1:5){
    train_dat <- rbind(train_dat, cipher_cluster[[i]]$centers[c])
  }
  
}


####### 3. Draw dendogram on the 50 clusters #######
dist_ciphers <-
  dist(train_dat,  method = "euclidean") # calculate distances
hc_ciphers <- hclust(dist_ciphers)
plot(hc_ciphers,
     main="2 Person Data kmeans 5 instances/digit",
     hang = -1,
     cex = 0.6,
     labels = train_lab)