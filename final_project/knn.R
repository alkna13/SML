#######################################################
#
# This scripts contains the code for the knn classfication
# for the final project in SML
#
#######################################################

## 1. Source data script ##
source("A:/Machine_Learning_Git/SML/final_project/data_processing.R")

## 2. Define preprocessing parameters ##
dpi = 300
smoothing = "none" # can be "gaussian" or "average" or "none"/NULL
sigma = 1 #only needed if smoothing = "gaussian"
x = loadSinglePerson(dpi,4,3,smoothing,sigma)

#Plot every cipher once
iterator = 400
for (c in 1:10){
  row = (iterator*c)-200
  
  imageToPlot = x[row,]
  textInfo = paste("Image of cipher ",imageToPlot[1]," with no smoothing")
  drawCipher(imageToPlot, textInfo)
}
