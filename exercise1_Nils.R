source('A:/Machine_Learning/Basefolder/loadImage.R') 

#####################################
# Exercise 1.4
# change the top 4 variables to do the different tasks
#####################################

test_split=0.5  #how large should the training set be 0.9=90/10 training/testing
runs=1          #how many times to run this loop with different random seeds
k_runs=1        #number of different k values to run
k_start=10      #starting k value. will just be the k used if k_runs=1

x=loadSinglePersonsData(300,4,2,"A:/Machine_Learning/2017/group") 

data_out<-array(0,dim=c(runs,k_runs,2))
#sd<-array(0,dim=c(10,2))

for(q in 1:runs)
{
  set.seed(990+q)
  dataset_shuffle <- x[sample(nrow(x)),]
  
  
  #create the training set
  dataset_train<- array(, dim=c((4000*test_split),3365))
  for(i in 1:dim(dataset_train)[1])
  {
    #kNN training set
    dataset_train[i,]<-dataset_shuffle[i,]
  }
  
  #create the testing set
  dataset_test<- array(, dim=c(4000-dim(dataset_train)[1],3365))
  for(i in 1:dim(dataset_test)[1])
  {
    #kNN testing set
    dataset_test[i,]<-dataset_shuffle[i+(4000*test_split),]
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

#use View(data_out[q,,]) to view specific run stats. column 1 is accuracy. replace q with a run number
#use View(data_out[,k,]) to view specific run stats. column 1 is accuracy. replace k with a desired k to view
#OR uncomment the one below to do all

#for(i in 1:q){View(data_out[i,,])}

#for(i in 1:k_runs){View(data_out[,i,])}
