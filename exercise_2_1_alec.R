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

#select which you want
x= loadSinglePersonsData(300,4,0,"C:/Users/Alec/Desktop/Uni/SML/SVN/2017/group")
#left to right the inputs are: (dpi,start_group,end_group,file_location)
#x_l= loadMultiplePersonsData(300,3,8,"C:/Users/Alec/Desktop/Uni/SML/SVN/2017/group")


#dataset_shuffle <- x[sample(nrow(x)),]
#dataset_shuffle <- x_l[sample(nrow(x_l)),] 
#dataset_shuffle<-x_l


dataset_0<- array(, dim=c(400,3365))
for(i in 1:400)
{
  #kNN testing set
  dataset_0[i,]<-x[i,]
}

pca<-prcomp(x)
pca<-prcomp(dataset_shuffle)

par(mar=rep(2,4))
plot(pca$sdev)
pcpercent=0
pc_desired=98
total_var=0
i=1
for(i in 1:3365)
{
  total_var<-total_var + pca$sdev[i]
}
print(total_var)
aaavar=0
i=0
acc_var=0
while(pcpercent<pc_desired)
#for(i in 1:3365)
{
  i=i+1
  acc_var<-acc_var+pca$sdev[i]
  pcpercent<-acc_var/(total_var/100)
  #print(pcpercent)
  #if(pcpercent>pc_desired){break}
  #pcpercent=pcpercent+pca$sdev[i]
  
}
print(i)

########################################################
#
# run this to acquire data
#
########################################################
loadData <- function()
{
  
}

########################################################
#
# 2.1.1
#
########################################################

exercise2_1_1 <- function()
{
  #change based on data set chosen
  pca<-prcomp(x)
}



########################################################
#
# 2.1.2
#
########################################################

exercise2_1_2 <- function()
{
  #eigenvector
  #columns of...
  
  
  #variance
  plot(pca$sdev)
  
  #accumulated variance absolute
  total_var=0                                         #maximum variance counter variable
  total_var_plot=array(,dim=c(1,length(pca$sdev)))    #array tracking accumulated total variance(absolute)
  for(i in 1:length(pca$sdev))
  {
    total_var<-total_var + pca$sdev[i]
    total_var_plot[i]<-total_var
  }
  plot(1:3365,total_var_plot,main="Accumulated Variance (absolute)",xlab="PC",ylab="Acumulated Variance")
  
  
  #accumulated variance percentage
  acc_var=0                                           #accumulated variance
  acc_var_plot=array(,dim=c(1,length(pca$sdev)))      #array tracking accumulated variance(%)
  for(i in 1:length(pca$sdev))
  {
    acc_var<-acc_var+pca$sdev[i]
    acc_var_plot[i]<-acc_var/(total_var/100)
  }
  plot(1:3365,acc_var_plot,main="Accumulated Variance (%)",xlab="PC",ylab="Acumulated Variance")
  
  
  #20 values output in array
  acc_var_out<-array(0,dim=c(20))
  for(i in 1:20)
  {
    acc_var_out[i]<-acc_var_plot[i*length(acc_var_out/20)]
  }
}



########################################################
#
# 2.1.3
#
########################################################

exercise2_1_3 <- function()
{
  
}


