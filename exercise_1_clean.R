source("C:/Users/Alec/Desktop/Uni/SML/exercise 1/loadImage.R")

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
        if(class(y)!="try-error")
        {
          x<-rbind2(y,x)
        }
      }
    }
  }
  options(show.error.messages = TRUE)
  return(x)
}

#######################################################
#
# Exercise 1.4
# change the top 4 variables to do the different tasks
#
#######################################################




test_split=0.5  #how large should the training set be 0.9=90/10 training/testing
runs=1          #how many times to run this loop with different random seeds
k_runs=1        #number of different k values to run
k_start=10      #starting k value. will just be the k used if k_runs=1

t_time<-proc.time()

#select which you want
#x= loadSinglePersonsData(300,4,0,"C:/Users/Alec/Desktop/Uni/SML/SVN/2017/group")
#left to right the inputs are: (dpi,start_group,end_group,file_location)
x_l= loadMultiplePersonsData(300,0,12,"C:/Users/Alec/Desktop/Uni/SML/SVN/2017/group")

data_out<-array(0,dim=c(runs,k_runs,2))


for(q in 1:runs)
{
  set.seed(990+q)
  
  #change data set here to/from x/x_l
  #dataset_shuffle <- x[sample(nrow(x)),]
  dataset_shuffle <- x_l[sample(nrow(x_l)),] 
  
  
  #create the training set
  dataset_train<- array(, dim=c((dim(dataset_shuffle)[1]*test_split),dim(dataset_shuffle)[2]))
  for(i in 1:dim(dataset_train)[1])
  {
    #kNN training set
    dataset_train[i,]<-dataset_shuffle[i,]
  }
  
  #create the testing set
  dataset_test<- array(, dim=c(dim=c((dim(dataset_shuffle)[1]-dim(dataset_train)[1]),dim(dataset_shuffle)[2])))
  for(i in 1:dim(dataset_test)[1])
  {
    #kNN testing set
    dataset_test[i,]<-dataset_shuffle[i+(dim(dataset_shuffle)[1]*test_split),]
  }
  
  #training set classification vector
  train_class<- array(, dim=c(1,dim(dataset_train)[1]))
  for(i in 1:dim(dataset_train)[1])
  {
    train_class[i]=dataset_train[i,1]
  }
  
  #testing set classification vector
  test_class<- array(, dim=c(1,dim(dataset_test)[1]))
  for(i in 1:dim(dataset_test)[1])
  {
    test_class[i]=dataset_test[i,1]
  }
  
  for(k in k_start:(k_start+k_runs-1))
  {
      
    #start timing
    ptm<-proc.time()
    
    #run knn test
    test_pred<-knn(dataset_train, dataset_test,train_class,k)

    
    #check accuracy
    correct=0
    incorrect=0
    for(i in 1:dim(dataset_test)[1])
    {
      if(test_pred[i]==test_class[i])
      {
        correct=correct + 1
      }
      else
      {
        incorrect = incorrect + 1
      }
    }
    accuracy=correct/dim(dataset_test)[1]*100
    #stop timing
    ptm<-proc.time()-ptm
    
    data_out[q,(k-k_start+1),1]=accuracy
    data_out[q,(k-k_start+1),2]=ptm[3]
    
  }
  
}
t_time<-proc.time()-t_time
cat("Total process time: ", t_time[3]/60, " minutes")
#use View(data_out[q,,]) to view specific run stats. column 1 is accuracy. replace q with a run number
#use View(data_out[,k,]) to view specific run stats. column 1 is accuracy. replace k with a desired k to view
#OR uncomment the one below to do all

#for(i in 1:q){View(data_out[i,,])}

#for(i in 1:k_runs){View(data_out[,i,])}
