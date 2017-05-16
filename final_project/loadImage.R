#-------------------------------------------------------------
#load libraries
#-------------------------------------------------------------
library("png")
library("EBImage")
library("class")
library("gmodels")
library("ggplot2")
library("caret")
#library("lda")
#library("neuralnet")
#library("RSNNS")
#install.packages("neuralnet")


#This file contains 2 functions and some example code in the bottom
#for using the 2 functions.
#The example code use the functions to load and smoothen the image data.
#lastly it converts the data into a table more suitable for R based classification 


#-------------------------------------------------------------
#Low pass smoothing function (standard)
#-------------------------------------------------------------
smoothImage <- function(grayImg){
  #two ways of specifying kernel:
  # kernel <- matrix( 
  #           c(1, 1, 1, 
  #             1, 1, 1, 
  #             1, 1, 1), # the data elements 
  #           3,              # number of rows 
  #           3)
  # kernel <- kernel/9
  # kernel
  kernel <- matrix( 
    1, # the data elements 
    3,# number of rows 
    3)
  kernel <- kernel/9
  #print(kernel)
  
  #using r library for smoothing
  smoothed <- filter2(grayImg, kernel)
  
  
  return(smoothed)
}

# Average smoothing (simple implementation)
smoothAverageImage <- function(gray){
  
  imgWidth <- length(gray[1,])
  imgHeight <- length(gray[,1])
  kernelSize <- 1
  for(px in 1:imgWidth)
  {
    for(py in 1:imgHeight)
    {
      baseX <- px - kernelSize
      endX <- px + kernelSize
      if(baseX < 1){baseX<-1}
      if(endX > imgWidth){endX<-imgWidth}
      
      baseY <- py - kernelSize
      endY <- py + kernelSize
      if(baseY < 1){baseY<-1}
      if(endY > imgHeight){endY<-imgHeight}
      
      
      value <- 0
      for(pkx in baseX:endX)
      {
        for(pky in baseY:endY)
        {
          value <- value+gray[pky,pkx]
        }
      }
      kernelValues <- (endY-baseY+1)*(endX-baseX+1)
      value <- value/kernelValues
      
      smoothed[py,px] <- value
    }
  }
  return(smoothed)
}

# Gaussian smoothing (EBImage lib)
gaussianBlur <- function(image, sigma){
  bluredImg = gblur(image, sigma)
  return(bluredImg)
  
}

#-------------------------------------------------------------
#Data loading function.
#This function load a single dataset where the data is
#specified by the DPI of the scanned images, the group number of the person
#and the number of the group member.
#Then the function use the smoothing function above, in order to smoothen the data.
#The function then places a grid around the digit cells, based on the corner file,
#so that it can be verified the data is read in a reasonable maner.
#Lastly a vector of table is returned. Each table in this vector represents
#a single type of digit, so the first table is only 0's, the second is 1's and so on.
#In the tables the rows represent the individual handwritten digits.
#The columns represents the pixel values.
#ADDED last parameter smoothingFunc. Possible values: "none","gaussian", "lowpass", "average"
#If smmoothFunc="gaussian" then gaussSigma must be a number
#-------------------------------------------------------------
loadSinglePersonsData <- function(DPI,groupNr,groupMemberNr,folder,smoothFunc,gaussSigma){
  #load the scaned images
  
  ciffers <- list(readPNG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-0.png"), collapse = "")),
                  readPNG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-1.png"), collapse = "")),
                  readPNG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-2.png"), collapse = "")),
                  readPNG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-3.png"), collapse = "")),
                  readPNG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-4.png"), collapse = "")))
  #load the corner values
  corners <- read.csv(paste(c(folder,groupNr,"/member",groupMemberNr,"/Corners.txt"), collapse = ""))
  corners <- trunc(corners*DPI/300)
  #print(corners)
  
  #define lists to be used
  #  gray <- list(1:5)
  #   smoothed <- list(1:5)
  prepared <- list(1:5)
  
  #convert the images to gray scale.
  for(i in 1:5)
  {
    if( length(dim(ciffers[[1]]) ) == 3 ) {
      r <-ciffers[[i]][,,1]
      g <-ciffers[[i]][,,2]
      b <-ciffers[[i]][,,3]
      prepared[[i]] <- (r+g+b)/3
    } else {
      prepared[[i]] <- ciffers[[i]]
    }  
  }

  #smooth images based on the funtion in the top
  for(i in 1:5)
  {
    
    #Use chosen smoothing on image (no smoothing, if none given)
    if (smoothFunc=='lowpass') prepared[[i]] <- smoothImage(prepared[[i]])
    else if (smoothFunc=='average')  prepared[[i]] <- smoothAverageImage(prepared[[i]])
    else if (smoothFunc=='gaussian')  prepared[[i]] <- gaussianBlur(prepared[[i]], gaussSigma)
    
  }  
 
  
  #extract individual ciffers
  #xStep and yStep is used to ensure the first corner of the
  #individual ciffers are placed fairly accurate
  xStep  <- (corners[1,7]-corners[1,1])/20;
  yStep  <- (corners[1,8]-corners[1,2])/20;
  
  #xStepT and yStepT is used to ensure that the feature vectors
  #from all people have the same size.
  
  xStepT <- 60*DPI/300
  yStepT <- 60*DPI/300
  
  dataMatrix <- matrix(1:((xStepT-2)*(yStepT-2) + 1)*10*20*20, nrow=10*20*20, ncol=(xStepT-2)*(yStepT-2) + 1)
  
  for(pages in 1:5)
  {
    for(box in 1:2)
    {
      for(cifX in 1:20)
      {
        aXbase <- corners[(pages-1)*2 + box,1] + xStep*(cifX-1)
        for(cifY in 1:20)
        {
          aYbase <- corners[(pages-1)*2 + box,2] + yStep*(cifY-1)
          
          dataMatrix[((pages-1)*2 + box - 1)*20*20 + (cifY-1)*20 + cifX ,1 ] <- (pages-1)*2 + box - 1
          
          for(px in 1:(xStepT-2))
          {
            for(py in 1:(yStepT-2))
            {
              #here things are read in
              dataMatrix[((pages-1)*2 + box - 1)*20*20 + (cifY-1)*20 + cifX ,1 + (px-1)*(yStepT-2) + py ] <- prepared[[pages]][aYbase+py+1,aXbase+px+1]
              
            }
          }
        }
      }
    }
  }
  
  return(dataMatrix)
}

###### Function to load multiple data ####
#ADDED last parameter smoothingFunc. Possible values: "none","gaussian", "lowpass", "average"
#If smmoothFunc="gaussian" then gaussSigma must be a Vector with different sigmas (eg. )
loadMultiplePersonsData <- function(dpi=300,startgrp=4,endgrp=4,location, smoothFunc, gaussSigma)
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


#-------------------------------------------------------------
# Function that draws one square (with Cipher) of the data 
#-------------------------------------------------------------
drawCipher <- function(oneCipherVector, title, subtitle){
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
  image(m,col = grey(seq(0, 1, length = 256)),sub=subtitle)  
  title(main = title, font.main = 4)
}


