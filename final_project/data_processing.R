#######################################################
#
# This scripts contains a method to load
# multiple data and methods for data processing
#
#######################################################
###### Libraries and absolute paths####
source("./loadImage.R")
pathToGroups = "A:/Machine_Learning/2017/group"

###### 1.Function to load multiple data ####
#ADDED last parameter smoothingFunc. Possible values: "none","gaussian", "lowpass", "average"
#If smmoothFunc="gaussian" then gaussSigma must be a Vector with different sigmas (eg. )
loadMultiplePersonsData <- function(dpi=300,startgrp=4,endgrp=4,smoothFunc, gaussSigma)
{
  options(show.error.messages = FALSE)
  memstart=0
  x=try(loadSinglePersonsData(dpi,startgrp,0,pathToGroups))#DPI change
  if(class(x)=="try-error")
  {
    x=loadSinglePersonsData(dpi,startgrp,1,pathToGroups)#DPI change
    memstart=1
  }
  
  for(i in startgrp:endgrp)
  {
    for(j in 0:4)
    {
      if(j!=memstart||i!=startgrp)
      {
        y=try(loadSinglePersonsData(dpi,i,j,pathToGroups))#DPI change
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

###### 2.Function to load single persons data ####
loadSinglePerson <- function(DPI,groupNr,groupMemberNr,smoothFunc,gaussSigma){
  return(loadSinglePersonsData(DPI,groupNr,groupMemberNr,pathToGroups,smoothFunc,gaussSigma))
}

#-------------------------------------------------------------
# Function that draws one square (with Cipher) of the data 
#-------------------------------------------------------------
drawCipher <- function(oneCipherVector, title){
  #Draw an image of one square
  #Square of one single cipher
  #initialize 
  columns = length(oneCipherVector)-1
  dimensions = sqrt(columns)
  cipherSquare = array(0, dim=c(dimensions,dimensions))
  
  x = 1
  iteration = 0 
  
  
  #get all values (but first one-> label for digit-value) of one row in data_test and save them in square
  for(col in 2:columns){
    y = col-(dimensions*iteration)
    cipherSquare[x,y] = oneCipherVector[col]
    
    #next row if dimension value of square is reached (last col-value in row)
    if(col%%dimensions==0){
      x = x + 1
      iteration = iteration + 1
    }
  }
  
  m = cipherSquare
  image(m,col = grey(seq(0, 1, length = 256)))  
  title(main = title, font.main = 4)
}
