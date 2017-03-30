source("/home/nils/Rfiles/loadImage.R")

#Libraries for plotting clusters
library(cluster)
library(fpc)


################################################################
#
# Exercise 3.1.1
#
# K-means clustering
#
################################################################

####### 1. Settings and preparation of training and test dataset #######

# First load two person data
x1 = loadSinglePersonsData(300,4,0,"/home/nils/2017/group") #0=Alec
x2 = loadSinglePersonsData(300,4,2,"/home/nils/2017/group") #2=Louis

dataset_train <- x1
#shuffle rows for test set
set.seed(995)
dataset_test <- x2[sample(nrow(x2)),]

####### 2. Perform k-means clustering on training data #######
#amount of clusters to perform knn on for each cipher
clusterSizes <- c(200,100,50,25)

for(c in 1:length(clusterSizes))
{
  clusterSize = clusterSizes[c]
  
  cipher_cluster <- c()
  label_cluster <- c()
  
  #for each cipher, define clusters
  for( i in 0:9) {
    #training data for cipher i (remove class-identifier in first col)
    train_data_i <- dataset_train[dataset_train[,1] == i, -1]
    #calculate kmeans with clusterSize clusters
    clusterData <- kmeans(train_data_i, clusterSize)
    
    #add center and label of the ith cipher to array
    cipher_cluster[[i + 1]] <- clusterData$centers
    label_cluster[[i + 1]] <- c(1:clusterSize)*0 + i
    # uncomment the following line to plot the cluster
    #plotcluster(train_data_i, clusterData$cluster)
    
    
  }
  
  rm(train_data_i)
  #get training-label and data for knn-algorithm out of clustered data
  train_lab <- factor(unlist(label_cluster))
  train_dat <- cipher_cluster[[1]]
  for( i in 2:10) {
    train_dat <- rbind(train_dat,cipher_cluster[[i]])
  }
  
  
  ################### TEST PLOTTING #######################################
  
  # plot(clusterData, col = clusterData$cluster)
  # points(clusterData$centers, col = 1:200, pch = 8, cex = 2)
  # 
  # 
  # plot(clusterData$centers,main="Cluster means", xlab="Cluster Mean",ylab="")
  # #function for rotating matrix by 90 degrees clockwise
  # rotate <- function(x) t(apply(x, 2, rev))
  # 
  # #row to plot to image
  # row = 1400
  # imageSize <- sqrt(ncol(train_dat)) #caculate dimensions of image
  # imageM <- matrix(train_dat[row, ], nrow = imageSize,ncol= imageSize, byrow = FALSE)
  # #imageM <- rotate(imageM)
  # title <- paste("Cipher ", train_lab[row], " at index ",row )
  # imageM <- rotate(imageM)
  # image(imageM, main = title)
  
  
  ################### PLOTTING END #######################################
  
  
  ####### 3. Perform knn on clustered training data  #######
  
  kstart=1
  kend=81
  kinc=5
  sample_size=(kend-kstart)/kinc+1
  
  cat(" variance  :  ", (kend-kstart)/kinc+1, " different k values \n")
  
  time_array <- array(0,dim=c(sample_size,2))
  time_array[,1]<- seq(from=kstart, to=kend, by=kinc)
  colnames(time_array ) <- c("k","time [sec]")
  
  performance_array <- array(0,dim=c((kend-kstart)/kinc+1,2))
  performance_array[,1]<- seq(from=kstart, to=kend, by=kinc)
  colnames(performance_array) <- c("k","Accuracy")
  
  #kloop
  for(k in seq(from=kstart, to=kend, by=kinc))
  {
    cat("Progress: ",(k-kstart)/(kend-kstart+1)*100,"% \n")
    #print(k)
    t_time<-proc.time()
    data_pred<-knn(train_dat, dataset_test[,2:3365],train_lab,k)
    t_time<-proc.time()-t_time
    
    time_array[(k-kstart)/kinc+1,2]<-t_time[3]
    
    cat("Progress: ",(k-kstart+kinc/2)/(kend-kstart+1)*100,"% for clusterSize=",c,"\n")
    
    #check accuracy unprocessed
    correct=0
    incorrect=0
    for(i in 1:dim(dataset_test)[1])
    {
      if(data_pred[i]==dataset_test[i,1])
      {
        correct=correct + 1
      }
      else
      {
        incorrect = incorrect + 1
      }
    }
    accuracy=correct/dim(dataset_test)[1]*100
    accuracy<-unname(accuracy)  #this is only because NAMED NUM annoyed me. its not necessary
    performance_array[(k-kstart)/kinc+1,2]<-accuracy
    
  }#end knn
  #for each cluster size: individual time and performance arrays
  cluster_name_time <- paste("time_array", clusterSize, sep = "")
  assign(cluster_name_time, time_array)
  cluster_name_performance <- paste("performance_array", clusterSize, sep = "")
  assign(cluster_name_performance, performance_array)
  
  #plot accuracy and time
  plot(performance_array,main=cluster_name_performance, xlab="k",ylab="Accuracy[%]")
  plot(time_array,main=cluster_name_time, xlab="k",ylab="time [sec]")
  
}#end clusterSize


################################################################
#
# Exercise 3.1.2
#
# KNN-comparison clustered data (see ex. 3.1.1) vs. raw data 
#
################################################################

####### 1. Perform knn on raw training data  #######
kstart=1
kend=81
kinc=5
sample_size=(kend-kstart)/kinc+1

cat(" variance  :  ", (kend-kstart)/kinc+1, " different k values \n")

time_array_raw <- array(0,dim=c(sample_size,2))
time_array_raw[,1]<- seq(from=kstart, to=kend, by=kinc)
colnames(time_array_raw ) <- c("k","time [sec]")

performance_array_raw <- array(0,dim=c((kend-kstart)/kinc+1,2))
performance_array_raw[,1]<- seq(from=kstart, to=kend, by=kinc)
colnames(performance_array_raw) <- c("k","Accuracy")

#kloop
for(k in seq(from=kstart, to=kend, by=kinc))
{
  cat("Progress: ",(k-kstart)/(kend-kstart+1)*100,"% \n")
  #print(k)
  t_time<-proc.time()
  data_pred<-knn(dataset_train[,2:3365], dataset_test[,2:3365], dataset_train[,1],k)
  t_time<-proc.time()-t_time
  
  time_array_raw[(k-kstart)/kinc+1,2]<-t_time[3]
  
  cat("Progress: ",(k-kstart+kinc/2)/(kend-kstart+1)*100,"% for clusterSize=",c,"\n")
  
  #check accuracy unprocessed
  correct=0
  incorrect=0
  for(i in 1:dim(dataset_test)[1])
  {
    if(data_pred[i]==dataset_test[i,1])
    {
      correct=correct + 1
    }
    else
    {
      incorrect = incorrect + 1
    }
  }
  accuracy=correct/dim(dataset_test)[1]*100
  accuracy<-unname(accuracy)  #this is only because NAMED NUM annoyed me. its not necessary
  performance_array_raw[(k-kstart)/kinc+1,2]<-accuracy
  
}#end knn

#plot accuracy and time
plot(performance_array_raw,main="Performance raw (2Person)", xlab="k",ylab="Accuracy[%]")
plot(time_array_raw,main="Time raw (2Person)", xlab="k",ylab="time [sec]")


################################################################
#
# Exercise 3.1.3
#
# K-means on entire class
#
################################################################

####### 1. Declare Function for loading entire class #######
loadMultiplePersonsData <- function(dpi=300,startgrp=4,endgrp=4,location)
{
  options(show.error.messages = FALSE)
  memstart=0
  x=try(loadSinglePersonsData(dpi,startgrp,0,location))#DPI change
  if(class(x)=="try-error")
  {
    x=loadSinglePersonsData(dpi,startgrp,1,location)#DPI change
    memstart=1
  }
  
  for(i in startgrp:endgrp)
  {
    for(j in 0:4)
    {
      if(j!=memstart||i!=startgrp)
      {
        y=try(loadSinglePersonsData(dpi,i,j,location))#DPI change
        if(class(y)!="try-error"&& y[1,1]==0)
        {
          x<-rbind2(y,x)
        }
      }
    }
  }
  options(show.error.messages = TRUE)
  return(x)
}

####### 2. Load data from entire class #######

# First load two person data
x_l= loadMultiplePersonsData(300,1,12,"/home/nils/2017/group")
test_split = 0.5 #how large should the training set be 0.9=90/10 training/testing
#no shuffling needed here, since the data is person independent (shuffle if person dependent)

#create the training set
dataset_train<- array(, dim=c((dim(x_l)[1]*test_split),dim(x_l)[2]))
for(i in 1:dim(dataset_train)[1])
{
  #kNN training set
  dataset_train[i,]<-x_l[i,]
}

#create the testing set
dataset_test<- array(, dim=c(dim=c((dim(x_l)[1]-dim(dataset_train)[1]),dim(x_l)[2])))
for(i in 1:dim(dataset_test)[1])
{
  #kNN testing set
  dataset_test[i,]<-dataset_shuffle[i+(dim(x_l)[1]*test_split),]
}
#just shuffle test-data so the same numbers dont come in a row (but I think it is not necessary) 
set.seed(990)
dataset_test <- dataset_test[sample(nrow(x_l)),]


####### 3. Perform kmeans clustering on entire class #######
#amount of clusters to perform knn on for each cipher
clusterSizes <- c(100,50,25)

for(c in 1:length(clusterSizes))
{
  clusterSize = clusterSizes[c]
  
  cipher_cluster <- c()
  label_cluster <- c()
  
  #for each cipher, define clusters
  for( i in 0:9) {
    #training data for cipher i (remove class-identifier in first col)
    train_data_i <- dataset_train[dataset_train[,1] == i, -1]
    #calculate kmeans with clusterSize clusters
    clusterData <- kmeans(train_data_i, clusterSize)
    
    #add center and label of the ith cipher to array
    cipher_cluster[[i + 1]] <- clusterData$centers
    label_cluster[[i + 1]] <- c(1:clusterSize)*0 + i
    # uncomment the following line to plot the cluster
    #plotcluster(train_data_i, clusterData$cluster)
    
    
  }
  
  rm(train_data_i)
  #get training-label and data for knn-algorithm out of clustered data
  train_lab <- factor(unlist(label_cluster))
  train_dat <- cipher_cluster[[1]]
  for( i in 2:10) {
    train_dat <- rbind(train_dat,cipher_cluster[[i]])
  }
}#end clustering


####### 4. Perform knn on entire clustered class #######
#amount of clusters to perform knn on for each cipher
clusterSizes <- c(400,200,100,50,25)

for(c in 1:length(clusterSizes))
{
  clusterSize = clusterSizes[c]
  
  cipher_cluster <- c()
  label_cluster <- c()
  
  #for each cipher, define clusters
  for( i in 0:9) {
    #training data for cipher i (remove class-identifier in first col)
    train_data_i <- dataset_train[dataset_train[,1] == i, -1]
    #calculate kmeans with clusterSize clusters
    clusterData <- kmeans(train_data_i, clusterSize)
    
    #add center and label of the ith cipher to array
    cipher_cluster[[i + 1]] <- clusterData$centers
    label_cluster[[i + 1]] <- c(1:clusterSize)*0 + i
    # uncomment the following line to plot the cluster
    #plotcluster(train_data_i, clusterData$cluster)
    
    
  }
  
  rm(train_data_i)
  #get training-label and data for knn-algorithm out of clustered data
  train_lab <- factor(unlist(label_cluster))
  train_dat <- cipher_cluster[[1]]
  for( i in 2:10) {
    train_dat <- rbind(train_dat,cipher_cluster[[i]])
  }
  
  
  ####### 4.1 Perform knn on clustered training data (entire class)  #######
  
  kstart=1
  kend=81
  kinc=5
  sample_size=(kend-kstart)/kinc+1
  
  cat(" variance  :  ", (kend-kstart)/kinc+1, " different k values \n")
  
  time_array <- array(0,dim=c(sample_size,2))
  time_array[,1]<- seq(from=kstart, to=kend, by=kinc)
  colnames(time_array ) <- c("k","time [sec]")
  
  performance_array <- array(0,dim=c((kend-kstart)/kinc+1,2))
  performance_array[,1]<- seq(from=kstart, to=kend, by=kinc)
  colnames(performance_array) <- c("k","Accuracy")
  
  #kloop
  for(k in seq(from=kstart, to=kend, by=kinc))
  {
    cat("Progress: ",(k-kstart)/(kend-kstart+1)*100,"% \n")
    #print(k)
    t_time<-proc.time()
    data_pred<-knn(train_dat, dataset_test[,2:3365],train_lab,k)
    t_time<-proc.time()-t_time
    
    time_array[(k-kstart)/kinc+1,2]<-t_time[3]
    
    cat("Progress: ",(k-kstart+kinc/2)/(kend-kstart+1)*100,"% for clusterSize=",c,"\n")
    
    #check accuracy unprocessed
    correct=0
    incorrect=0
    for(i in 1:dim(dataset_test)[1])
    {
      if(data_pred[i]==dataset_test[i,1])
      {
        correct=correct + 1
      }
      else
      {
        incorrect = incorrect + 1
      }
    }
    accuracy=correct/dim(dataset_test)[1]*100
    accuracy<-unname(accuracy)  #this is only because NAMED NUM annoyed me. its not necessary
    performance_array[(k-kstart)/kinc+1,2]<-accuracy
    
  }#end knn
  #for each cluster size: individual time and performance arrays with unique name
  cluster_name_time <- paste("time_array", clusterSize,"entireClass", sep = "_")
  assign(cluster_name_time, time_array)
  cluster_name_performance <- paste("performance_array", clusterSize,"entireClass", sep = "_")
  assign(cluster_name_performance, performance_array)
  
  #plot accuracy and time
  plot(performance_array,main=cluster_name_performance, xlab="k",ylab="Accuracy[%]")
  plot(time_array,main=cluster_name_time, xlab="k",ylab="time [sec]")
}#end clusterSize
