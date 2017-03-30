source("/home/nils/Rfiles/loadImage.R")
source("A:/Machine_Learning/Basefolder/loadImage.R")

#Libraries for plotting clusters
library(cluster)
library(fpc)


################################################################
#
# Exercise 3.2.1
#
# Dendogram with 5 instances of each digit (on one person)
#
################################################################

####### 1. Obtain data #######

# Load one person data
#x1 = loadSinglePersonsData(300,4,0,"/home/nils/2017/group") #0=Alec (amazon server path)
x1 = loadSinglePersonsData(300,4,0,"A:/Machine_Learning/2017/group") #0=Alec (local PC path)
#array for the cipher instances
cipher_array <- array(0,dim=c(50,dim(x1)[2]))

####### 2. Get five instances of each cipher out of training set #######
for(cipher_iter in 1:10)
{
  cipher = cipher_iter-1
  
  for(i in 1:5){
    row = (cipher*5)+i
    row_train_set = (cipher*400)+i
    
    cipher_array[row,] <-x1[row_train_set,]
    cat("Row: ",row, " values= ", cipher_array[row,1:100],"\n")
    
  }
}
####### 2. Calculate distances and draw dendogram #######
dist_ciphers <- dist(cipher_array[,2:dim(x1)[2]],  method = "euclidean") # calculate distances
hc_ciphers <- hclust(dist_ciphers)
plot(hc_ciphers, hang = -1, cex = 0.6)

