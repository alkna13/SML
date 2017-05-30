#### Script description ###################################################
#
# This scripts contains the code for the knn classfication
# for the final project in SML
#
#######################################################

#### 1. Source data script ####
source("A:/Machine_Learning_Git/SML/final_project/loadImage.R")
library(reshape2)
library(ggplot2)

#### 2. Define knn method ####
#xPca are the first x values that should be used for knn (if Pcas were used)
#saveResults is a boolean defining if results should be saved to output folder
doKnn <- function(xPca, saveResults) {
  total_knn_start_time <- proc.time()
  
  k = k_start
  k_run = 0 #iterator for k-runs
  while (k <= k_end)
  {
    #if PCAs are used:
    if(pre_pca){
      print (paste("knn for k=", k, " is being calculated ...", "| first PCA: ", xPca))
      k_run <- k_run + 1
      #start timing
      ptm <- proc.time()
      
      #run knn test
      test_pred <<-
        knn(dataset_train[, 2:(firstXPCAs[xPca] + 1)], dataset_test[,  2:(firstXPCAs[xPca] +
                                                                            1)], train_class, k)
      
      #stop timing
      ptm <- proc.time() - ptm
    }else{
      #if no PCAs are used
      print (paste("knn for k=", k, " is being calculated ..."))
      k_run <- k_run + 1
      #start timing
      ptm <- proc.time()
      
      #run knn test
      test_pred <<-
        knn(dataset_train[, 2:dim(dataset_train)[2]], dataset_test[,  2:dim(dataset_train)[2]], train_class, k)
      
      #stop timing
      ptm <- proc.time() - ptm
    }
    
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
    row <- k_run
    
    data_out[row, 11] <<- k
    data_out[row, 12] <<- accuracy
    data_out[row, 13] <<- ptm[3]
    
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
    c("Accuracy_Avg",
      "Variance",
      "StdDev",
      "Time_Avg",
      "Time_Total")
  total_values[1, 1] <- accAvg
  total_values[1, 2] <- varAcc
  total_values[1, 3] <- stdev
  total_values[1, 4] <- timeAvg
  total_values[1, 5] <- timeTotal
  if(saveResults){
    #plot confusion matrix
    confus.knn.res <- confusionMatrix(test_pred, dataset_test[,1])
    print(confus.knn.res$table)
    
    #plot graph with accuracy
    #preparation
    results <- as.data.frame(data_out)
    results <- results[order(results$k),] #order
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
    if(pre_pca){
      #file names if pca was applied
      ggsave(filename = paste(outputFolderCross, "/plot_knn_PCAs_",firstXPCAs[xPca],".png", sep = ""))
      textFilename1 = paste(outputFolderCross, "/all_results_PCAs_", firstXPCAs[xPca], ".csv", sep = "")
      textFilename2 = paste(outputFolderCross, "/total_results_PCAs_",firstXPCAs[xPca],".csv", sep = "")
      textFilename3 = paste(outputFolderCross, "/confusion_matrix_PCAs_",firstXPCAs[xPca],".csv", sep = "")
      
    }else{
      #file names if pca was NOT applied
      ggsave(filename = paste(outputFolderCross, "/plot_knn.png", sep = ""))
      textFilename1 = paste(outputFolderCross, "/all_results_knn.csv", sep = "")
      textFilename2 = paste(outputFolderCross, "/total_results_knn.csv", sep = "")
      textFilename3 = paste(outputFolderCross, "/confusion_matrix.csv", sep = "")
    }
    write.csv2(data_out, file = textFilename1) # save file for results of each k
    write.csv2(total_values, file = textFilename2) # save file for total sum results
    write.csv2(confus.knn.res$table, file= textFilename3)
  }
  return(total_values)
}# end knn-function

#### 3. Define preprocessing parameters, folders and data source ####
#use preprocessed folder (DONT RENAME THE FOLDER) for centering(automatic corner and contour detection with rotation and centering automation)
pathToGroups = "A:/Machine_Learning/preProcessed/2017/group"
#output folder to put all result documents in
outputFolder = "crossval_allClass_PersonDependent_k4"

dpi = 100
smoothing = "gaussian" # can be "gaussian" or "average" or "none"/NULL
sigma = 0.6 #only needed if smoothing = "gaussian"
pre_pca = FALSE
normalization = FALSE

#set group and#set start group and end group (number or "all" for all)
group = "all"
member = "all" 

#print loaded Ciphers from 0-9 once if TRUE and saves it in output Folder
printCiphers=FALSE

#create new ouput Folder
dir.create(outputFolder)

personsNrTotal=35 #number of persons in class

#### 4. Define KNN parameters ####
#parameters
test_split = 0.9  #how large should the training set be 0.9=90/10 training/testing
person_dependent = TRUE #if multiple persons used (p.dep.= vertical split, p.indep. horizontal split)
k_inc = 1  #value by k will be incremented after each run
k_start = 4   #starting k value. will just be the k used if k_runs=1
k_end = 4
k_runs = floor((k_end - k_start) / k_inc) + 1
print (paste(k_runs, " runs of k will be run"))


#k-fold runs
crossRun = 0
amountOfCrossRuns = 10 #k-fold number

#### 5. Load data ####

#start measuring time
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


#draw Ciphers
if(printCiphers){
  for (c in 1:10) {
    row = (iterator * c) - 200
    
    imageToPlot = x[row,]
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
}

#initialize array for output of crossvalidation
crossValid_results <- array(,dim=c(amountOfCrossRuns+1,6))

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

#### 5b. Define Cross validation parameters ####
# define increment (length of test part either of whole dataset or one person)
if (member != "all" | person_dependent == FALSE){
  #PERSON INDEPENDENT
  crossIncrementer = (1-test_split)* dim(x)[1]
} else {
  #PERSON DEPENDENT
  crossIncrementer = (1-test_split)* 4000 #4000 rows for one person 
}


##### 6. Prepare dataset ####
dataset <- x
#create the training set
dataset_train <-
  array(, dim = c((dim(dataset)[1] * test_split), dim(dataset)[2]))
#create the testing set
dataset_test <-
  array(, dim = c(dim = c((
    dim(dataset)[1] - dim(dataset_train)[1]
  ), dim(dataset)[2])))



##### 6a. Begin Cross validation loop ####
while( crossRun < amountOfCrossRuns ){
  #set start end end of test set and left training set
  test.start = crossIncrementer*crossRun + 1
  test.end = (crossRun+1)*crossIncrementer
  
  #training set left of test set
  train1.start =  1
  train1.end = test.start-1
  
  
  #Create folder for each cross run
  outputFolderCross <- paste(outputFolder,"/cross_run_",(crossRun+1),sep="")
  dir.create(outputFolderCross)
  
  
#fill dependent on chosen persons and split method
if (member != "all" | person_dependent == FALSE) {
  #For single persons or person independent data:
  set.seed(995)
  dataset <- dataset[sample(nrow(dataset)),]
 
 
  #check for cross valid run nr
  if(crossRun == amountOfCrossRuns-1 | crossRun==0){
    #if last or first crossRun (no first/second trains set)
    if(crossRun == 0){
      train1.start = 0
      train1.end <- 0
      train2.start =  test.end +1
      train2.end = dim(dataset)[1]
      dataset_train <- dataset[train2.start:train2.end,]
      dataset_test <- dataset[test.start:test.end,]
    }else if (crossRun == amountOfCrossRuns-1){
      train2.start <- 0
      train2.end <- 0
      dataset_train <- dataset[train1.start:train1.end,]
      dataset_test <- dataset[test.start:test.end,]
    }
    
  }else{
    #if NOT last or first crossRun
    #training set right of test set
    train2.start =  1 + test.end
    train2.end = dim(dataset)[1]
    #fill both training sets
    trainset1 <- dataset[train1.start:train1.end,]
    trainset2 <- dataset[train2.start:train2.end,]
    dataset_train = rbind(trainset1,trainset2)
    dataset_test <- dataset[test.start:test.end,]
  }
  
  

} else{
  #For multiple persons and person dependent:
  personRows = 4000 #amount of rows for each user
  persons = dim(dataset)[1] / personRows #amount of persons
  
  
  for (p in 1:persons) {
    #define rows of person
    rowStart = (p * personRows) - personRows + 1
    rowEnd = p * personRows
    dataset_person <-
      dataset[rowStart:rowEnd, ] #dataset of one person
    
    #shuffle data
    set.seed(995)
    dataset_person <- dataset_person[sample(nrow(dataset_person)),]
    #Each shuffled dataset of one person has to be cross validated
    #check for cross valid run nr
    if(crossRun == amountOfCrossRuns-1 | crossRun==0){
      #if last or first crossRun (no first/second trains set)
      if(crossRun == 0){
        train1.start = 0
        train1.end <- 0
        train2.start =  test.end +1
        train2.end = dim(dataset_person)[1]
        #initialize training and test dataset
       
        dataset_train_p <- dataset_person[train2.start:train2.end,]
        dataset_test_p <- dataset_person[test.start:test.end,]
      }else if (crossRun == amountOfCrossRuns-1){
        train2.start <- 0
        train2.end <- 0
        dataset_train_p <- dataset_person[train1.start:train1.end,]
        #rest is test set
        dataset_test_p <- dataset_person[test.start:test.end,]
        
      }
      
    }else{
      #if NOT last or first crossRun
      #training set right of test set
      train2.start =  1 + test.end
      train2.end = dim(dataset_person)[1]
      #fill both training sets
      trainset1 <- dataset_person[train1.start:train1.end,]
      trainset2 <- dataset_person[train2.start:train2.end,]
      dataset_train_p = rbind(trainset1,trainset2)
      #rest is test set
      dataset_test_p <- dataset_person[test.start:test.end,]
      
      
    }
    if(p==1){
      #override testset the first time
      dataset_train <- dataset_train_p
      dataset_test <- dataset_test_p
    }else{
      #bind train and test dataset to whole dataset 
      dataset_train <- rbind(dataset_train,dataset_train_p)
      dataset_test <- rbind(dataset_test,dataset_test_p)
    }
    
    
  }
  
  #shuffle training and test set seperately again for whole set
  set.seed(995)
  dataset_train <- dataset_train[sample(nrow(dataset_train)),]
  dataset_test <- dataset_test[sample(nrow(dataset_test)),]
  
  rm(dataset_train_p, dataset_test_p)
  rm(rowStart, rowEnd, dataset_person)
}
  print(paste("Length train/test-set: ",dim(dataset_train)[1],"/",dim(dataset_test)[1]))
#Print status about cross validation
print(paste("crossRun",crossRun,"| test.start:",test.start,", test.end:",test.end,"| train1.start:",train1.start,"train1.end:",
              train1.end, "| train2.start:",train2.start,", train2.end:",train2.end))

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



  #### 7. Begin knn ####
  #use previous found maximum and save results to output folder
  
  print("Begin knn (if pca used: with pca max)")
  
  if(pre_pca){
    total_knn_results <- doKnn(pca_max,FALSE)
  }else{
    total_knn_results <- doKnn(0,TRUE)
  }
#save results of knn to cross validation array (crossRun begins at 0, thats why +1)
crossValid_results[(crossRun + 1),1] <- crossRun + 1
crossValid_results[(crossRun + 1),2:6] <- total_knn_results[1,]
#increment cross validation run
crossRun = crossRun + 1
}#end cross valid loop


#### 8. Save total results of cross validation ####

#Name columns of cross results
colnames(crossValid_results) <-
  c("cross_run_nr","Accuracy_Avg",
    "Variance",
    "StdDev",
    "Time_Avg",
    "Sum_Time_Total")

#Calculate total average results for all cross validation runs
#and save those to last row
df <- as.data.frame(crossValid_results[1:amountOfCrossRuns,])
print("Results Crossvalidation:")
print(df)


row = amountOfCrossRuns+1
crossValid_results[row,1] <- 'all'
crossValid_results[row,2] <- as.numeric(mean(df$Accuracy_Avg))
crossValid_results[row,3] <- as.numeric(mean(df$Variance))
crossValid_results[row,4] <- as.numeric(mean(df$StdDev))
crossValid_results[row,5] <- as.numeric(mean(df$Time_Avg))
crossValid_results[row,6] <- as.numeric(sum(df$Sum_Time_Total))



#Save results of cross validation to file
textFilenameCross = paste(outputFolder, "/all_results_crossVal.csv", sep = "")
write.csv2(crossValid_results, file = textFilenameCross) 