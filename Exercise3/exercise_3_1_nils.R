source("A:/Machine_Learning/Basefolder/loadImage.R")

################################################################
#
# Exercise 3.1.1
#
# K-means clustering
#
################################################################

####### 1. Settings and preparation of training and test dataset #######
test_split=0.5  #how large should the training set be 0.9=90/10 training/testing


# First load two person data
x1 = loadSinglePersonsData(300,4,3,"A:/Machine_Learning/2017/group")
x2 = loadSinglePersonsData(300,4,2,"A:/Machine_Learning/2017/group")

#shuffle rows 
set.seed(990)
dataset_shuffle1 <- x1[sample(nrow(x1)),]
set.seed(995)
dataset_shuffle2 <- x2[sample(nrow(x2)),]

#create the training set
dataset_train<- array(, dim=c((dim(dataset_shuffle1)[1]*test_split*2),dim(dataset_shuffle1)[2])) #*2 for 2 persons
for(i in 1:dim(dataset_train)[1])
{
  #fill first part of kNN training set with person1 and then second part with person2
  if(i < dim(dataset_train)[1]/2){
    dataset_train[i,]<-dataset_shuffle1[i,]
  }else{
    dataset_train[i,]<-dataset_shuffle2[i/2,]
  }
}

#create the testing set
dataset_test<- array(, dim=c(dim=c((dim(dataset_shuffle1)[1]*2 - dim(dataset_train)[1]),dim(dataset_shuffle1)[2])))

for(i in 1:dim(dataset_test)[1])
{
  #fill first part of kNN test set with person1 and then second part with person2
  if(i < dim(dataset_test)[1]/2){
    dataset_test[i,]<-dataset_shuffle1[i+(dim(dataset_shuffle1)[1]*test_split),]
  }else{
    dataset_test[i,]<-dataset_shuffle2[i/2+(dim(dataset_shuffle2)[1]*test_split),]
  }
}

# remove shuffled datasets
rm(dataset_shuffle1)
rm(dataset_shuffle2)

#training set classification vector (first column)
train_class<- array(, dim=c(1,dim(dataset_train)[1]))
for(i in 1:dim(dataset_train)[1])
{
  train_class[i]=dataset_train[i,1]
}

#testing set classification vector (first column)
test_class<- array(, dim=c(1,dim(dataset_test)[1]))
for(i in 1:dim(dataset_test)[1])
{
  test_class[i]=dataset_test[i,1]
}

####### 2. Perform k-means clustering on training data #######

cipher_cluster <- c()
label_cluster <- c()

#for each cipher, define clusters
for( i in 0:9) {
  clusterData <- kmeans(dataset_train[ dataset_train[1:4000,1] == i, ], 200)
  cipher_cluster[[i + 1]] <- clusterData$centers
  label_cluster[[i + 1]] <- c(1:200)*0 + i
}
train_lab <- factor(unlist(label_cluster))
train_dat <- cipher_cluster[[1]]
for( i in 2:10) {
  train_dat <- rbind(train_dat,cipher_cluster[[i]])
}


####### 3. Perform knn on clustered training data #######
kstart=1
kend=80
kinc=1
sample_size=(kend-kstart)/kinc+1

cat(" variance  :  ", (kend-kstart)/kinc+1, " different k values \n")

time_array <- array(0,dim=c(sample_size,3))
time_array[,1]<- seq(from=kstart, to=kend, by=kinc)

performance_array <- array(0,dim=c((kend-kstart)/kinc+1,3))
performance_array[,1]<- seq(from=kstart, to=kend, by=kinc)

for(k in seq(from=kstart, to=kend, by=kinc))
  #for(k in kstart:(kstart+kruns-1))
{
  cat("Progress: ",(k-kstart)/(kend-kstart+1)*100,"% \n")
  #print(k)
  t_time<-proc.time()
  data_pred<-knn(train_dat, dataset_test,train_lab,k)
  t_time<-proc.time()-t_time
  
  time_array[(k-kstart)/kinc+1,2]<-t_time[3]
  
  cat("Progress: ",(k-kstart+kinc/2)/(kend-kstart+1)*100,"% \n")
  
  #check accuracy unprocessed
  correct=0
  incorrect=0
  for(i in 1:dim(dataset_test)[1])
  {
    if(data_pred[i]==test_class[i])
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

#plot accuracy
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
