#### Script description ###################################################
#
# This scripts contains the code for the knn classfication
# for the final project in SML
#
#######################################################

#### 1. Source data script ####
source("preProcessing_runs/loadImage.R")
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
    
    #save all k data to summary array --> see last step
    rowAll <- ((personNr*k_runs)-k_runs+row)
    data_out_all[rowAll,] <<- data_out[row,]
    
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
      ggsave(filename = paste(outputFolderPerson, "/plot_knn_PCAs_",firstXPCAs[xPca],".png", sep = ""))
      textFilename1 = paste(outputFolderPerson, "/all_results_PCAs_", firstXPCAs[xPca], ".csv", sep = "")
      textFilename2 = paste(outputFolderPerson, "/total_results_PCAs_",firstXPCAs[xPca],".csv", sep = "")
      textFilename3 = paste(outputFolderPerson, "//confusion_matrix_PCAs_",firstXPCAs[xPca],".csv", sep = "")
      
    }else{
      #file names if pca was NOT applied
      ggsave(filename = paste(outputFolderPerson, "/plot_knn.png", sep = ""))
      textFilename1 = paste(outputFolderPerson, "/all_results_knn.csv", sep = "")
      textFilename2 = paste(outputFolderPerson, "/total_results_knn.csv", sep = "")
      textFilename3 = paste(outputFolderPerson, "/confusion_matrix.csv", sep = "")
    }
    write.csv2(data_out, file = textFilename1) # save file for results of each k
    write.csv2(total_values, file = textFilename2) # save file for total sum results
    write.csv2(confus.knn.res$table, file= textFilename3)
  }
  return(total_values)
}# end knn-function






#### 3. Define preprocessing parameters, folders and data source ####
#use preprocessed folder (DONT RENAME THE FOLDER) for centering(automatic corner and contour detection with rotation and centering automation)
pathToGroups = "allData/preProcessed/2017/group"
#output folder to put all result documents in
outputFolder = "test_save_files"

dpi = 100
smoothing = "gaussian" # can be "gaussian" or "average" or "none"/NULL
sigma = 0.6 #only needed if smoothing = "gaussian"
pre_pca = FALSE
normalization = FALSE

#set start group and  member
group = 0 #put in number for single group to start with
member = 1 #put in a nr for single person to start with

#print loaded Ciphers from 0-9 once if TRUE and saves it in output Folder
printCiphers=TRUE

#create new ouput Folder
dir.create(outputFolder)

personsNrTotal=35 #number of persons in class
all_persons_results <-array(,dim=c(personsNrTotal,7))


personNr = 1 #person to calculate
endReached = FALSE #will be checked at the end of following while loop

#### 4. Define KNN parameters ####
#parameters
test_split = 0.8  #how large should the training set be 0.9=90/10 training/testing
person_dependent = TRUE #if multiple persons used (p.dep.= vertical split, p.indep. horizontal split)
k_inc = 1  #value by k will be incremented after each run
k_start = 1   #starting k value. will just be the k used if k_runs=1
k_end = 30
k_runs = floor((k_end - k_start) / k_inc) + 1
print (paste(k_runs, " runs of k will be run"))

#Define array for all Persons and all ks
data_out_all <- array(0, dim = c(k_runs*personsNrTotal, 13))


#### 5. Load data (Start of Loop) ####
while(!endReached &&member<3){
  print(paste("Start calculating person",personNr))
  #Create folders for person
  dir.create(paste(outputFolder,"/Group",group,sep=""))
  outputFolderPerson <- paste(outputFolder,"/Group",group,"/member",member, sep="")
    dir.create(outputFolderPerson)
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
    dev.copy(png, filename = paste(outputFolderPerson, "/cipher", c - 1, ".png", sep =
                                     ""))
    dev.off ()
  }
}


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

#fill dependent on chosen persons and split method
#For single persons or person independent data:
if (member != "all" | person_dependent == FALSE) {
  set.seed(995)
  dataset <- dataset[sample(nrow(dataset)),]
  
  
  for (i in 1:dim(dataset_train)[1])
  {
    #kNN training set
    dataset_train[i,] <- dataset[i,]
  }
  
  
  for (i in 1:dim(dataset_test)[1])
  {
    #kNN testing set
    dataset_test[i,] <-
      dataset[i + (dim(dataset)[1] * test_split),]
  }
} else{
  #For multiple persons and person dependent:
  personRows = 4000 #amount of rows for each user
  trainDataRows = personRows * test_split
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
    #add to training set
    dataset_train_p <- dataset_person[1:trainDataRows, ]
    #add to test set
    dataset_test_p <- dataset_person[1:(personRows - trainDataRows), ]
    
    #Add rows of person to whole training and test set
    startRowTrain = (p * trainDataRows) - trainDataRows + 1
    startRowTest = (p * (personRows - trainDataRows)) - (personRows - trainDataRows) +
      1
    dataset_train[startRowTrain:(startRowTrain + trainDataRows - 1), ] <-
      dataset_train_p
    dataset_test[startRowTest:(startRowTest + (personRows - trainDataRows) -
                                 1), ] <- dataset_test_p
  }
  #shuffle training and test set seperately
  set.seed(995)
  dataset_train <- dataset_train[sample(nrow(dataset_train)),]
  dataset_test <- dataset_test[sample(nrow(dataset_test)),]
  
  rm(dataset_train_p, dataset_test_p)
  rm(rowStart, rowEnd, dataset_person)
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


#### 6b. Do PCA and normalization (optional) ####
if (pre_pca) {
  start.time <- proc.time()
  #leave out first column --> class identifier
  pca_train <- prcomp(dataset_train[, 2:dim(dataset_train)[2]])
  #apply the same manipullation on test set
  pca_test <-
    dataset_test[, 2:dim(dataset_test)[2]] %*% pca_train$rotation
  #save time for PCA
  end.time.knn <- proc.time() - start.time
  print(paste("Time for PCA: ", end.time.knn[3]))
  
  dataset_train <-
    array(, dim = c(dim(dataset_train)[1], (dim(pca_train$x)[2] + 1)))
  dataset_test <-
    array(, dim = c(dim(dataset_test)[1], (dim(pca_test)[2] + 1)))
  
  if (normalization) {
    start.time <- proc.time()
    dataset_train[, 2:dim(dataset_train)[2]] <-
      normalize(pca_train$x)
    dataset_test[, 2:dim(dataset_test)[2]] <-  normalize(pca_test)
    #save time for norm
    end.time.norm <- proc.time() - start.time
    print(paste("Time for normalisation: ", end.time.norm[3]))
    #add time in output array
    data_out[, 9] = paste("TRUE (t=", end.time.norm[3], "s")
  } else{
    dataset_train [, 2:dim(dataset_train)[2]] <- pca_train$x
    dataset_test [, 2:dim(dataset_test)[2]] <- pca_test
  }
  
  #add class identifiers again
  dataset_train[, 1] <- train_class
  dataset_test[, 1] <- test_class
  #name the columns
  colnames(dataset_train) <-
    c("cipher", paste("PCA", 1:(dim(dataset_train)[2] - 1)))
  colnames(dataset_test) <-
    c("cipher", paste("PCA", 1:(dim(dataset_test)[2] - 1)))
  #add time in output array
  data_out[, 8] = paste("TRUE (t=", end.time.knn[3], "s")
  
  
  
  #Evaluate PCA
  subText = paste("Group: ", group, "member: ", member, "| Split:", splitText)
  #variance
  variances <- c(1, length(pca_train$sdev))
  for (i in 1:length(pca_train$sdev))
  {
    variances[i] <- (pca_train$sdev[i]) ^ 2
  }
  
  plot(
    variances,
    main = "Variance by PCA (training data)",
    xlab = "PCA",
    ylab = "Variance",
    sub = subText
  )
  dev.copy(png,
           filename = paste(outputFolderPerson, "/variance_per_PCA.png", sep = ""))
  dev.off ()
  write.csv2(variances,
             file = paste(outputFolderPerson, "/variance_per_PCA.csv", sep = "")) # save file for all results
  
  #accumulated variance absolute
  total_var = 0                                         #maximum variance counter variable
  total_var_plot = array(, dim = c(1, length(variances)))    #array tracking accumulated total variance(absolute)
  for (i in 1:length(pca_train$sdev))
  {
    total_var <- total_var + variances[i]
    total_var_plot[i] <- total_var
  }
  
  
  #plot and save accumulated Variance
  
  plot(
    x = 1:length(total_var_plot),
    y = total_var_plot,
    main = "Accumulated Variance (absolute, training)",
    xlab = "PC",
    ylab = "Acumulated Variance",
    sub = subText
  )
  dev.copy(png, filename = paste(outputFolderPerson, "/acc_variance.png", sep =
                                   ""))
  dev.off ()
  write.csv2(total_var_plot,
             file = paste(outputFolderPerson, "/acc_variance.csv", sep = "")) # save file for all results
  
  #accumulated variance percentage
  acc_var = 0                                           #accumulated variance
  acc_var_plot = array(, dim = c(1, length(variances)))      #array tracking accumulated variance(%)
  for (i in 1:length(variances))
  {
    acc_var <- acc_var + variances[i]
    acc_var_plot[i] <- acc_var / (total_var / 100)
  }
  plot(
    1:length(acc_var_plot),
    acc_var_plot,
    main = "Accumulated Variance (%, training)",
    xlab = "PC",
    ylab = "Acumulated Variance [%]",
    sub = subText
  )
  dev.copy(png,
           filename = paste(outputFolderPerson, "/acc_variance_perc.png", sep = ""))
  dev.off ()
  write.csv2(total_var_plot,
             file = paste(outputFolderPerson, "/acc_variance_perc.csv", sep = "")) # save file for all results
} # end of pca

#remove unnecessary variables
rm(x)


#### 6c. Find optimal # of first PCAs for  knn (if PCA is used)####
#vector with different # of PCAs to use in each run
if(pre_pca){
  firstXPCAs <- c(2:101)
  
  k_start_old <- k_start
  k_end_old <-k_end
  k_runs_old <- k_runs
  
  k_inc = 1  #value by k will be incremented after each run
  k_start = 10    #starting k value. will just be the k used if k_runs=1
  k_end = 10
  k_runs = 1
  results_firstXPCAs <- array(, dim = c(100, 5))
  for (xPca in 20:length(firstXPCAs)) {
    print("find maximum first xPca")
    results_firstXPCAs[xPca,] <- doKnn(xPca,TRUE) # dont save results to output folder
  }
  colnames(results_firstXPCAs) <-
    c("Accuracy_Avg",
      "Variance",
      "StdDev",
      "Time_Avg",
      "Time_Total")
  textFilenameTEMP = paste(outputFolderPerson, "/total_results_firstXPCAs.csv", sep = "")
  write.csv2(results_firstXPCAs, file = textFilenameTEMP) # save file for total sum results
  pca_max <- which.max(results_firstXPCAs[,1])
  
  k_start <- k_start_old
  k_end <-k_end_old
  k_runs <- k_runs_old
  rm(k_start_old,k_runs_old,k_end_old,k_start_old)
}
#### 7. Begin knn ####
#use previous found maximum and save results to output folder

print("Begin knn (if pca used: with pca max)")

if(pre_pca){
  total_knn_results <- doKnn(pca_max,FALSE)
}else{
  total_knn_results <- doKnn(0,TRUE)
}

all_persons_results[personNr,1] <-group
all_persons_results[personNr,2] <- member
all_persons_results[personNr,3:7] <-total_knn_results



#### 8. Check if all persons are calculated ####
#increase member see if it is exists
exists = FALSE
member <- member+1
path <- paste(pathToGroups,group,"/member",member,sep="")

while(!dir.exists(file.path(path))&&member<10){
  member <- member+1
  path <- paste(pathToGroups,group,"/member",member,sep="")
}
if(member<10){
  exists = TRUE  
}else{
  #no mor member in group, so increase group

  #if it doesnt exist, go to next group
  member <- 0
  group <- group+1
  path <- paste(pathToGroups,group,"/member",member,sep="")
  if(!dir.exists(file.path(path))){
    #iterate member# if there is any member between 0-10 in the next group
    while(!dir.exists(file.path(path))&&member<10){
      member <- member+1
      path <- paste(pathToGroups,group,"/member",member,sep="")
    }
    #if the next group and/or no members in it exist, all persons data were loaded
    if(member>=10) endReached=TRUE
  }
}
personNr = personNr+1
}# end while loop one single user

#### 9. Save summary tables ####
colnames(all_persons_results) <-
  c("group","person","Accuracy_Avg",
    "Variance",
    "StdDev",
    "Time_Avg",
    "Time_Total")
colnames(data_out_all) <-
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

textFilenameSummary= paste(outputFolder, "/allPersonsResults.csv", sep = "")
textFilenameAllKs= paste(outputFolder, "/allPersonsResultsWithAllK.csv", sep = "")
write.csv2(all_persons_results, file = textFilenameSummary) # save file for total sum results
write.csv2(data_out_all, file = textFilenameAllKs) # save file for total sum result
