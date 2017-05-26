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

#use preprocessed folder (DONT RENAME THE FOLDER) for centering(automatic corner and contour detection with rotation and centering automation)
pathToGroups = "A:/Machine_Learning/2017/group"
#output folder to put all result documents in
outputFolder = "pca1"

#### 2. Define preprocessing parameters ####
dpi = 300
smoothing = "none" # can be "gaussian" or "average" or "none"/NULL
sigma = 0 #only needed if smoothing = "gaussian"
pre_pca = TRUE
normalization = TRUE
group = 4 #put in number for single group or "all" for whole course
member = "all" #put in a nr for single person or "all" for whole group

#### 3. Load data ####
start.time <- proc.time()

#load all course, all members of one group or one person
if (group == "all") {
  x = loadMultiplePersonsData(dpi, 0, 12, pathToGroups, smoothing, sigma)#mult. person
} else if (member == "all") {
  x = loadMultiplePersonsData(dpi, group, group, pathToGroups, smoothing, sigma)#mult. person
} else{
  x = loadSinglePersonsData(dpi, group, member, pathToGroups, smoothing, sigma) #1Person
}
data.loading.time <- proc.time() - start.time
print(paste("Time for loading data: ", data.loading.time[3]))

#Plot every cipher once
iterator = 400
#prepare text for image title (smoothing method)
if (smoothing == "gaussian") {
  smoothingText = paste(" gaussian with sigma=", sigma)
} else {
  smoothingText = smoothing
}

if (member == "all") {
  memberText = "first of group"
} else{
  memberText = member
}

#was centered data loaded?
centeredText = isTRUE(grepl("preProcessed", pathToGroups))#check if preProcessed folder was used

#create new ouput Folder
dir.create(outputFolder)

#draw Ciphers
for (c in 1:10) {
  row = (iterator * c) - 200
  
  imageToPlot = x[row, ]
  textInfo = paste("Image of cipher ",
                   imageToPlot[1],
                   "(Group: ",
                   group,
                   " member: ",
                   memberText,
                   ")")
  subText = paste("Smoothing:",
                  smoothingText,
                  ", dpi:",
                  dpi,
                  ", centering:",
                  centeredText)
  img <- drawCipher(imageToPlot, textInfo, subText)
  dev.copy(png, filename = paste(outputFolder, "/cipher", c - 1, ".png", sep =
                                   ""))
  dev.off ()
  
  
}

#### 4. Define KNN parameters ####
#parameters
test_split = 0.8  #how large should the training set be 0.9=90/10 training/testing
person_dependent = TRUE #if multiple persons used (p.dep.= vertical split, p.indep. horizontal split)
k_inc = 1  #value by k will be incremented after each run
k_start = 10    #starting k value. will just be the k used if k_runs=1
k_end = 10
k_runs = floor((k_end - k_start) / k_inc) + 1
print (paste(k_runs, " will be run"))

#Initialize and fill output table
data_out <- array(0, dim = c(k_runs, 13))
colnames(data_out) <-
  c(
    "Group_#",
    "Member#",
    "DPI",
    "Centered",
    "Smoothing",
    "Gaussian_Sigma",
    "Data_Loading_Time [s]",
    "PCA",
    "Normalized",
    "split",
    "k",
    "accuracy",
    "k_time [s]"
  )

data_out[, 1] = group
data_out[, 2] = member
data_out[, 3] = dpi
data_out[, 4] = centeredText
data_out[, 5] = smoothing
if (smoothing == "gaussian") {
  data_out[, 6] = sigma
} else{
  data_out[, 6] = 0
}
data_out[, 7] = data.loading.time[3]
data_out[, 8] = pre_pca
data_out[, 9] = normalization



if (person_dependent) {
  splitText = paste(test_split, " (Person dependent)")
} else{
  splitText = paste(test_split, " (Person independent)")
}
data_out[, 10] = splitText

#5. Prepare dataset
dataset <- x
#create the training set
dataset_train <-
  array(, dim = c((dim(dataset)[1] * test_split), dim(dataset)[2]))
#create the testing set
dataset_test <-
  array(, dim = c(dim = c((
    dim(dataset)[1] - dim(dataset_train)[1]
  ), dim(dataset)[2])))

#fill dependent on chosen persons and split method
#For single persons or person independent data:
if (member != "all" | person_dependent == FALSE) {
  set.seed(995)
  dataset <- dataset[sample(nrow(dataset)), ]
  
  
  for (i in 1:dim(dataset_train)[1])
  {
    #kNN training set
    dataset_train[i, ] <- dataset[i, ]
  }
  
  
  for (i in 1:dim(dataset_test)[1])
  {
    #kNN testing set
    dataset_test[i, ] <- dataset[i + (dim(dataset)[1] * test_split), ]
  }
} else{
  
  #For multiple persons and person dependent:
  personRows = 4000 #amount of rows for each user
  trainDataRows = personRows*test_split
  persons = dim(dataset)[1] / personRows #amount of persons
  for (p in 1:persons) {
    
    #define rows of person
    rowStart = (p*personRows)-personRows+1
    rowEnd = p*personRows 
    dataset_person <- dataset[rowStart:rowEnd,] #dataset of one person
    
    #shuffle data
    set.seed(995)
    dataset_person <- dataset_person[sample(nrow(dataset_person)), ]
    #add to training set
    dataset_train_p <- dataset_person[1:trainDataRows,]
    #add to test set
    dataset_test_p <- dataset_person[1:(personRows-trainDataRows),]
    
    #Add rows of person to whole training and test set
    startRowTrain = (p*trainDataRows)-trainDataRows+1
    startRowTest = (p*(personRows-trainDataRows))-(personRows-trainDataRows)+1
    dataset_train[startRowTrain:(startRowTrain+trainDataRows-1),]<- dataset_train_p
    dataset_test[startRowTest:(startRowTest+(personRows-trainDataRows)-1),]<- dataset_test_p
  }
  #shuffle training and test set seperately
  set.seed(995)
  dataset_train <- dataset_train[sample(nrow(dataset_train)), ]
  dataset_test <- dataset_test[sample(nrow(dataset_test)), ]
  
  rm(dataset_train_p, dataset_test_p)
  rm(rowStart,rowEnd, dataset_person)
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


#### 5. Do PCA and normalization (optional) ####
if (pre_pca) {
  start.time <- proc.time()
  #leave out first column --> class identifier
  pca_train <- prcomp(dataset_train[, 2:dim(dataset_train)[2]])
  #apply the same manipullation on test set
  pca_test <- dataset_test[,2:dim(dataset_test)[2]] %*% pca_train$rotation
  #save time for PCA
  end.time.knn <- proc.time()-start.time
  print(paste("Time for PCA: ",end.time.knn[3]))
  
  dataset_train <- array(,dim=c(dim(dataset_train)[1],(dim(pca_train$x)[2]+1)))
  dataset_test <- array(,dim=c(dim(dataset_test)[1],(dim(pca_test)[2]+1)))
  
  if (normalization) {
    start.time <- proc.time()
    dataset_train[,2:dim(dataset_train)[2]] <- normalize(pca_train$x)
    dataset_test[,2:dim(dataset_test)[2]] <-  normalize(pca_test)
    #save time for norm
    end.time.norm <- proc.time()-start.time
    print(paste("Time for normalisation: ",end.time.norm[3]))
    #add time in output array
    data_out[, 9] = paste("TRUE (t=",end.time.norm[3],"s")
  } else{
    dataset_train [,2:dim(dataset_train)[2]] <- pca_train$x
    dataset_test [,2:dim(dataset_test)[2]]<- pca_test
  }
  
  #add class identifiers again
  dataset_train[,1] <- train_class
  dataset_test[,1] <- test_class
  #name the columns
  colnames(dataset_train)<-c("cipher",paste("PCA", 1:(dim(dataset_train)[2]-1)))
  colnames(dataset_test)<-c("cipher",paste("PCA", 1:(dim(dataset_test)[2]-1)))
  #add time in output array
  data_out[, 8] = paste("TRUE (t=",end.time.knn[3],"s")
  
  
  
  #Evaluate PCA
  subText = paste("Group: ",group,"member: ", member,"| Split:", splitText)
  #variance
  variances <- c(1,length(pca_train$sdev))
  for(i in 1:length(pca_train$sdev))
  {
    variances[i]<-(pca_train$sdev[i])^2
  }
  
  plot(variances,main="Variance by PCA (training data)", xlab="PCA",ylab="Variance", sub=subText)
  dev.copy(png, filename = paste(outputFolder, "/variance_per_PCA.png", sep =""))
  dev.off ()
  write.csv2(variances, file = paste(outputFolder,"/variance_per_PCA.csv",sep="")) # save file for all results
  
  #accumulated variance absolute
  total_var=0                                         #maximum variance counter variable
  total_var_plot=array(,dim=c(1,length(variances)))    #array tracking accumulated total variance(absolute)
  for(i in 1:length(pca_train$sdev))
  {
    total_var<-total_var + variances[i]
    total_var_plot[i]<-total_var
  }
  
  
  #plot and save accumulated Variance
  
  plot(x=1:length(total_var_plot),y=total_var_plot,main="Accumulated Variance (absolute, training)",xlab="PC",ylab="Acumulated Variance",sub = subText)
  dev.copy(png, filename = paste(outputFolder, "/acc_variance.png", sep =""))
  dev.off ()
  write.csv2(total_var_plot, file = paste(outputFolder,"/acc_variance.csv",sep="")) # save file for all results
  
  #accumulated variance percentage
  acc_var=0                                           #accumulated variance
  acc_var_plot=array(,dim=c(1,length(variances)))      #array tracking accumulated variance(%)
  for(i in 1:length(variances))
  {
    acc_var<-acc_var+variances[i]
    acc_var_plot[i]<-acc_var/(total_var/100)
  }
  plot(1:length(acc_var_plot),acc_var_plot,main="Accumulated Variance (%, training)",xlab="PC",ylab="Acumulated Variance [%]", sub=subText)
  dev.copy(png, filename = paste(outputFolder, "/acc_variance_perc.png", sep =""))
  dev.off ()
  write.csv2(total_var_plot, file = paste(outputFolder,"/acc_variance_perc.csv",sep="")) # save file for all results
} # end of pca

#remove unnecessary variables
rm(x)


#### 6. Begin knn ####
#vector with different # of PCAs to use in each run
firstXPCAs<- c(50,100,200,400,600,1000,2000)
for(f in 1:length(firstXPCAs)){
  
  
  
  total_knn_start_time <- proc.time()
  
  k = k_start
  k_run = 0 #iterator for k-runs
  while (k <= k_end)
  {
    print (paste("knn for k=", k, " is being calculated ..."))
    k_run <- k_run + 1
    #start timing
    ptm <- proc.time()
    
    #run knn test
    test_pred <-
      # RESET LATER: knn(dataset_train[, 2:dim(dataset_train)[2]], dataset_test[,2:dim(dataset_test)[2]], train_class, k)
      knn(dataset_train[, 2:(firstXPCAs[f]+1)], dataset_test[,2:(firstXPCAs[f]+1)], train_class, k)
    #stop timing
    ptm <- proc.time() - ptm
    
    #check accuracy
    correct = 0
    incorrect = 0
    for (i in 1:dim(dataset_test)[1])
    {
      if (test_pred[i] == test_class[i])
      {
        correct = correct + 1
      }
      else
      {
        incorrect = incorrect + 1
      }
    }
    accuracy = correct / dim(dataset_test)[1] * 100
    
    
    #fill last 3 columns of output array
    row = (k_run)
    data_out[row, 11] = k
    data_out[row, 12] = accuracy
    data_out[row, 13] = ptm[3]
    
    #increase k
    k = k + k_inc
  }
  
  
  total_time <- proc.time() - total_knn_start_time
  cat("Total process time: ", total_time[3] / 60, " minutes")
  
  #calculate total values
  accuracies <- as.numeric(data_out[, 12])
  times <- as.numeric(data_out[, 13])
  total_values <- array(0, c(1, 5))
  accAvg = mean(accuracies)
  varAcc = (sum((accuracies - accAvg) ^ 2)) / length(accuracies)
  stdev = sqrt(varAcc)
  timeAvg = mean(times)
  timeTotal = sum(times)
  
  colnames(total_values) <-
    c("Accuracy_Avg", "Variance", "StdDev", "Time_Avg", "Time_Total")
  total_values[1, 1] <- accAvg
  total_values[1, 2] <- varAcc
  total_values[1, 3] <- stdev
  total_values[1, 4] <- timeAvg
  total_values[1, 5] <- timeTotal
  
  #plot graph with accuracy
  #preparation
  results <- as.data.frame(data_out)
  results <- results[order(results$k), ] #order
  k <- results$k
  accuracy <- results$accuracy
  performance <- results$`k_time [s]`
  df <- data.frame(k, accuracy, performance)
  df.melted <- melt(df, id = "k")
  
  ggplot(data = df.melted, aes(x = reorder(k), y = value, color = variable)) +
    geom_point() +
    labs(title = "Accuracy and Performance for knn", x = "k", y = "Value\n(Performances: Time in sec | Accuracy: Percentage %)") +
    facet_grid(variable ~ ., scales = 'free')
  
  # Save table and plots
  ggsave(filename = paste(outputFolder, "/plot_knn_PCAs",firstXPCAs[f],".png", sep = ""))
  textFilename1 = paste(outputFolder, "/all_results_PCAs",firstXPCAs[f],".csv", sep = "")
  textFilename2 = paste(outputFolder, "/total_results_PCAs",firstXPCAs[f],".csv", sep = "")
  
  write.csv2(data_out, file = textFilename1) # save file for results of each k
  write.csv2(total_values, file = textFilename2) # save file for total sum results
}