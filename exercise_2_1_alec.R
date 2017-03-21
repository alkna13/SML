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

################################################################
#
# Exercise 2.1
#
# Execute the functions at the bottom to 
# perfom the individual problems
#
################################################################

#person independent- person not included in training set





#################################################################
#
# Load data into dataframes
#
#################################################################


  #select which you want. load only once. takes forever
  x= loadSinglePersonsData(300,4,0,"C:/Users/Alec/Desktop/Uni/SML/SVN/2017/group")
  #left to right the inputs are: (dpi,start_group,end_group,file_location)
  x_l= loadMultiplePersonsData(300,3,8,"C:/Users/Alec/Desktop/Uni/SML/SVN/2017/group")




########################################################
#
# 2.1.1
# load data and apply pca
#
########################################################

  
  
  
  
  #test person mixed in is person dependent
  test_mixed=TRUE
  use_multi=TRUE
  test_split=0.9  #how large should the training set be 0.9=90/10 training/testing
 
    set.seed(990)
    
  
    
    if(use_multi)
    {
      if(test_mixed)
      {
        dataset_shuffle <- x_l[sample(nrow(x_l)),]
      } else
      {
        dataset_shuffle <-x_l
      }
    } else
    {
      dataset_shuffle <- x[sample(nrow(x)),]
      
    }
    
    
    
    
    
    
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
    
    print("Beginning pca on dataset")
    #pca<-prcomp(dataset_shuffle)
    rm(dataset_shuffle)
    
    
    pca_time<-proc.time()
    
    pca_train<-prcomp(dataset_train, center=FALSE)
    
    #pca_test<-prcomp(dataset_test)
    pca_test<-dataset_test %*% pca_train$rotation
    
    pca_time<-proc.time()-pca_time
    #time is third variable
  
  





########################################################
#
# 2.1.1
#
########################################################

  #eigenvector
  #columns of...
  
  
  #variance
  plot(pca_train$sdev,main="Variance by PCA", xlab="PCA",ylab="Variance")
  
  #accumulated variance absolute
  total_var=0                                         #maximum variance counter variable
  total_var_plot=array(,dim=c(1,length(pca_train$sdev)))    #array tracking accumulated total variance(absolute)
  for(i in 1:length(pca_train$sdev))
  {
    total_var<-total_var + pca_train$sdev[i]
    total_var_plot[i]<-total_var
  }
  plot(x=1:length(total_var_plot),y=total_var_plot,main="Accumulated Variance (absolute)",xlab="PC",ylab="Acumulated Variance")
  
  
  #accumulated variance percentage
  acc_var=0                                           #accumulated variance
  acc_var_plot=array(,dim=c(1,length(pca_train$sdev)))      #array tracking accumulated variance(%)
  for(i in 1:length(pca_train$sdev))
  {
    acc_var<-acc_var+pca_train$sdev[i]
    acc_var_plot[i]<-acc_var/(total_var/100)
  }
  plot(1:length(acc_var_plot),acc_var_plot,main="Accumulated Variance (%)",xlab="PC",ylab="Acumulated Variance")
  
  
  #20 values output in array
  acc_var_out<-array(0,dim=c(20))
  for(i in 1:20)
  {
    acc_var_out[i]<-acc_var_plot[i*length(acc_var_out/20)]
  }




########################################################
#
# 2.1.2/3
#
########################################################


  kstart=10
  kend=100
  kinc=5
  desired_percent=90
  sample_size=(kend-kstart)/kinc+1
  
  cat(desired_percent, " percent variance  :  ", (kend-kstart)/kinc+1, " different k values \n")
  
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
    data_pred<-knn(dataset_train, dataset_test,train_class,k)                            #uncomment
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

    
    #processed data
    
    #determine number of PCs
    total_var=0                                         #maximum variance counter variable
    for(i in 1:length(pca_train$sdev))                        #determine total variance
    {
      total_var<-total_var + pca_train$sdev[i]
    }
    
    #accumulated variance percentage
    acc_var=0                                           #accumulated variance
    pc_i=0
    #for(i in 1:length(pca$sdev))
    while((acc_var/total_var)<(desired_percent/100))
    {
      pc_i<-pc_i+1
      acc_var<-acc_var+pca_train$sdev[pc_i]
    }
    
    #cut data
    pca_train_red<- array(,dim=c(dim(pca_train$x)[1],pc_i))
    pca_test_red<- array(,dim=c(dim(pca_test)[1],pc_i))
    for(i in 1:pc_i)
    {
      pca_train_red[,i] <- pca_train$x[,i]
      pca_test_red[,i] <- pca_test[,i]
    }
    
    
    #process the data
    p_time<-proc.time()
    pca_pred<-knn(pca_train_red, pca_test_red,train_class,k)
    p_time<-proc.time()-p_time
    
    time_array[(k-kstart)/kinc+1,3]=p_time[3]
    
    
    
    #check accuracy pca
    correct=0
    incorrect=0
    for(i in 1:dim(dataset_test)[1])
    {
      if(pca_pred[i]==test_class[i])
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
    performance_array[(k-kstart)/kinc+1,3]<-accuracy
    
    
    
  }


# time_array columns are in order: k, unprocessed knn time, pca knn time, pca adjusted with preprocessing time added
  
  
  
  
### Exercise 2.3 Reconstruction using PCA
  #Rotate matrix by 90 degrees clockwise
  rotate <- function(x) t(apply(x, 2, rev))
  
# 1. Draw one of each cipher
  
  #row iteration
  row = 1
  step = 400 #because each cipher is repeated 400 times
  
  imageSize <- sqrt(ncol(dataset_test)-1) #caculate dimensions of image
  
  #Plot all images from 0-9
  for(i in 1:10){
    imageM <- matrix(x[row,2:ncol(x)],nrow = imageSize, ncol
                      = imageSize,byrow = FALSE)
    imageM <- rotate(imageM) 
    image( imageM )
    row = row + step
  }
  
  #2. Plot eigenvectors
  for(i in 1:10){
    
    testEigenvector = pca_train$rotation[2:((imageSize^2)+1),i]
    imageM <- matrix(testEigenvector,nrow = imageSize, ncol
                     = imageSize,byrow = FALSE)
    imageM <- rotate(imageM) 
    image( imageM, col = grey(seq(0, 1, length = 256)))
    }
  #3. Plot reconstruction of original images by using all PCs
  allCiphers = array()
  allCiphersDrawn = FALSE
  iteration = 0
  while(!allCiphersDrawn){
    iteration = iteration+1
    trunc <- pca_train$x[iteration,1:nrow(pca_train$rotation)] %*%
      t(pca_train$rotation[,1:nrow(pca_train$rotation)])
    
      currentCipher <-trunc[1,1]
    #check if cipher is already drawn
      alreadyDrawn = FALSE
    for(r in 1:nrow(allCiphers)){
      if(allCiphers[] && allCiphers[r]== currentCipher){
        alreadyDrawn = TRUE
        break
      }
    }
    # if not drawn, add it to ciphers draw it  
    if(!alreadyDrawn){
      allCiphers[nrow(allCiphers)] <- currentCipher
      
      imageM <- matrix(trunc[,2:3365],nrow = imageSize, ncol
                       = imageSize,byrow = FALSE)
      #trunc <- scale(trunc, center = -1 * pca_train$center, scale=FALSE)
      imageM <- rotate(imageM) 
      image( imageM)
      
      # check if it was the last drawn cipher
      if(allCiphers && nrow(allCiphers)== 10) allCiphersDrawn = TRUE
    
    }  
  }