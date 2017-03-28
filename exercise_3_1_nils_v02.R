source("A:/Machine_Learning/Basefolder/loadImage.R")

################################################################
#
# Exercise 3.1.1
#
# K-means clustering
#
################################################################

####### 1. Settings and preparation of training and test dataset #######

# First load two person data
x1 = loadSinglePersonsData(300,4,0,"A:/Machine_Learning/2017/group") #0=Alec
x2 = loadSinglePersonsData(300,4,2,"A:/Machine_Learning/2017/group") #2=Louis


dataset_train <- x1
#shuffle rows for test set
set.seed(995)
dataset_test <- x2[sample(nrow(x2)),]

####### 2. Perform k-means clustering on training data #######
#amount of values for each cluster
clusterSize = 200

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
for(k in seq(from=kstart, to=kend, by=kinc))
  #for(k in kstart:(kstart+kruns-1))
{
  cat("Progress: ",(k-kstart)/(kend-kstart+1)*100,"% \n")
  #print(k)
  t_time<-proc.time()
  data_pred<-knn(train_dat, dataset_test[,2:3365],train_lab,k)
  t_time<-proc.time()-t_time
  
  time_array[(k-kstart)/kinc+1,2]<-t_time[3]
  
  cat("Progress: ",(k-kstart+kinc/2)/(kend-kstart+1)*100,"% \n")
  
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
  
}

#plot accuracy and time
plot(performance_array,main="Accuracy", xlab="k",ylab="Accuracy[%]")
plot(time_array,main="Time", xlab="k",ylab="time [sec]")

################################################################
#
# Exercise 3.1.2
#
# KNN-comparison clustered data vs. raw data (should be taken from one of previous exercises)
#
################################################################



################################################################
#
# Exercise 3.1.3
#
# K-means on entire class
#
################################################################
