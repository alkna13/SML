#######################################################
#
# This scripts contains the code for the knn classfication
# for the final project in SML
#
#######################################################

#### 1. Source data script ####
source("A:/Machine_Learning_Git/SML/final_project/loadImage.R")
library(reshape2)
library(ggplot2)

#use preprocessed folder for centering(automatic corner and contour detection with rotation and centering automation)
pathToGroups = "A:/Machine_Learning/2017/group"
#output folder to put all result documents in
outputFolder="test1"

#### 2. Define preprocessing parameters ####
dpi = 300
smoothing = "none" # can be "gaussian" or "average" or "none"/NULL
sigma = 1 #only needed if smoothing = "gaussian"
pre_pca = FALSE
normalization = FALSE
group = 4 #put in number for single group or "all" for whole course
member = 0 #put in a nr for single person or "all" for whole group

#### 3. Load data ####
start.time <- proc.time()

#load all course, all members of one group or one person
if(group=="all"){
  x = loadMultiplePersonsData(dpi, 0, 12, pathToGroups, smoothing, sigma)#mult. person
}else if(member=="all"){
  x = loadMultiplePersonsData(dpi, group, group, pathToGroups, smoothing, sigma)#mult. person
}else{
  x = loadSinglePersonsData(dpi,group,member,pathToGroups,smoothing,sigma) #1Person
}


data.loading.time <- proc.time() - start.time
print(paste("Time for loading data: ", data.loading.time[3]))

#Plot every cipher once
iterator = 400
#prepare text for image title (smoothing method)
if(smoothing =="gaussian"){
  smoothingText = paste(" gaussian with sigma=",sigma)
}else {
  smoothingText = smoothing
}

if(member=="all"){
  memberText = "first of group"
}else{
  memberText = member
}

#was centered data loaded?
centeredText = isTRUE(grepl( "preProcessed",pathToGroups))#check if preProcessed folder was used

#create new ouput Folder
dir.create(outputFolder)

#draw Ciphers
for (c in 1:10){
  row = (iterator*c)-200
  
  imageToPlot = x[row,]
  textInfo = paste("Image of cipher ",imageToPlot[1],"(Group: ",group," member: ", memberText,")")
  subText = paste("Smoothing:",smoothingText,", dpi:",dpi,", centering:",centeredText )
  img <- drawCipher(imageToPlot, textInfo, subText)
  dev.copy(png,filename=paste(outputFolder,"/cipher",c-1,".png",sep=""))
  dev.off ()
  
  
}


#### 4. Shuffle data and do PCA/normalization (if necessary) ####
set.seed(990)
dataset_shuffled <- x[sample(nrow(x)),]

## Do PCA and normalization if necessary
if(pre_pca){
  #leave out first column --> class identifier
  pca <- prcomp(dataset_shuffle[,2:dim(dataset_shuffle)[2]])
  dataset <-pca$x
  if(normalization){
    #do normalization
    dataset <- scale(pca$x)
  }
  
} else {
  #dataset is just shuffled data
  dataset <-dataset_shuffled
}


#### 5. Begin KNN ####
#parameters
test_split=0.5  #how large should the training set be 0.9=90/10 training/testing
person_dependent = FALSE
runs=1          #how many times to run this loop with different random seeds
k_inc=5        #value by k will be incremented after each run
k_start=1      #starting k value. will just be the k used if k_runs=1
k_end=10
k_runs = floor((k_end+1-k_start)/k_inc)

#Initialize and fill output table
data_out<-array(0,dim=c(runs*k_runs,13))
colnames(data_out)<- c("Group_#","Member#","DPI","Centered","Smoothing","Gaussian_Sigma",
                       "Data_Loading_Time [s]","PCA","Normalized","split","k","accuracy","k_time [s]")

data_out[,1] = group
data_out[,2] = member
data_out[,3] = dpi
data_out[,4] = centeredText
data_out[,5] = smoothing
if(smoothing=="gaussian"){
  data_out[,6] = sigma  
}else{
  data_out[,6] = 0
}

data_out[,7] = data.loading.time[3]
data_out[,8] = pre_pca
data_out[,9] = normalization
if(person_dependent){
  splitText=cat(split, " (Person dependent)")
} else{
  splitText=cat(split, " (Person independent)")
} 
data_out[,10] = splitText


total_knn_start_time <- proc.time()
for(runNr in 1:runs)
{
  
  #create the training set
  dataset_train<- array(, dim=c((dim(dataset)[1]*test_split),dim(dataset)[2]))
  for(i in 1:dim(dataset_train)[1])
  {
    #kNN training set
    dataset_train[i,]<-dataset[i,]
  }
  
  #create the testing set
  dataset_test<- array(, dim=c(dim=c((dim(dataset)[1]-dim(dataset_train)[1]),dim(dataset)[2])))
  for(i in 1:dim(dataset_test)[1])
  {
    #kNN testing set
    dataset_test[i,]<-dataset[i+(dim(dataset)[1]*test_split),]
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
  k=k_start
  k_run = 0 #iterator for k-runs
  while(k<=k_end)
  {
   
   k_run <- k_run+1
    #start timing
    ptm<-proc.time()
    
    #run knn test
    test_pred<-knn(dataset_train, dataset_test,train_class,k)#check first col of train data
    
    #stop timing
    ptm<-proc.time()-ptm
    
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
    
    
    #fill last 3 columns of output array
    row = (k_run)+(runNr-1)*(k_runs)
    data_out[row,11]= k
    data_out[row,12]= accuracy
    data_out[row,13]= ptm[3]
    
    #increase k
    k=k+k_inc
  }
  
}
total_time<-proc.time()-total_knn_start_time
cat("Total process time: ", total_time[3]/60, " minutes")

#calculate total values
accuracies <- as.numeric(data_out[,12])
times <- as.numeric(data_out[,13])
total_values <- array(0,c(1,5))
accAvg = mean(accuracies)
varAcc = (sum((accuracies-accAvg)^2))/length(accuracies)
stdev = sqrt(varAcc)
timeAvg = mean(times)
timeTotal = sum(times)

colnames(total_values)<-c("Accuracy_Avg","Variance","StdDev","Time_Avg","Time_Total")
total_values[1,1] <- accAvg
total_values[1,2] <- varAcc
total_values[1,3] <- stdev
total_values[1,4] <- timeAvg
total_values[1,5] <- timeTotal

#plot graph with accuracy
#preparation
results <- as.data.frame(data_out)
results <- results[order(results$k),] #order
k <- results$k
accuracy <- results$accuracy
performance <- results$`k_time [s]`
df <- data.frame(k, accuracy, performance)
df.melted <- melt(df, id = "k")

ggplot(data = df.melted, aes(x=reorder(k), y = value, color = variable)) + 
  geom_point() +
  labs(title="Accuracy and Performance for knn", x="k", y="Value\n(Performances: Time in sec | Accuracy: Percentage %)") + 
  facet_grid(variable ~ ., scales='free')

# Save table and plots
ggsave(filename=paste(outputFolder,"/plot_knn.png",sep=""))
textFilename1 = paste(outputFolder,"/all_results.csv",sep="")
textFilename2 = paste(outputFolder,"/total_results.csv",sep="")
  
write.csv2(data_out,file=textFilename1) # save file for all results
write.csv2(total_values, file=textFilename2) # save file for total sum results
           