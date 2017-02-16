# FOR THE LOVE OF GOD, DO NOT USE THE RUN BUTTON. JUST PRESS CTRL+R
library(class)

source("C:/Users/Alec/Desktop/Uni/SML/SVN/Basefolder/loadImage.R")

q=0
x= loadSinglePersonsData(300,4,0,"C:/Users/Alec/Desktop/Uni/SML/SVN/2017/group")
avg_data<-array(0,dim=c(10,2))
sd<-array(0,dim=c(10,2))
for(q in 1:10)
{
set.seed(990+q)
dataset_shuffle <- x[sample(nrow(x)),]


# Exercise 1.4.4
# smoothImage <- function(greyImg){
#   kernel = makeBrush(3, shape = "Gaussian", step = TRUE, sigma = 0.55)
#   print(kernel)
#   smoothed= filter2(greyImg, kernal)
#   return(smoothed)
#   }

dataset_train<- array(, dim=c(2000,3365))
for(i in 1:2000)
{
  #kNN training set
  dataset_train[i,]<-dataset_shuffle[i,]
}

dataset_test<- array(, dim=c(2000,3365))
for(i in 1:2000)
{
  #kNN testing set
  dataset_test[i,]<-dataset_shuffle[i+2000,]
}

cl<- array(, dim=c(1,2000))
for(i in 1:2000)
{
  cl[i]=dataset_train[i,1]
}

dl<- array(, dim=c(1,2000))
for(i in 1:2000)
{
  dl[i]=dataset_test[i,1]
}


p=80
stats_out<-array(,dim=c(p,2))
#K FOR KNN TEST
k=10

for(k in 10:p)
{
ptm<-proc.time()
test_pred<-knn(dataset_train, dataset_test,cl,k)

#CrossTable(dl,test_pred,prop.chisq = FALSE)
correct=0
incorrect=0
for(i in 1:2000)
{
  if(test_pred[i]==dl[i])
  {
    correct=correct + 1
  }
  else
  {
    incorrect = incorrect + 1
  }
}
ptm<-proc.time()-ptm
stats_out[k,2]=correct/2000*100
stats_out[k,1]=ptm[3]
#avg_data[q,2]=correct/400*100
#avg_data[q,1]=ptm[3]
}

}

sd2=array(0,dim=c(1,2))
time_avg=0
acc_avg=0
for(w in 1:10)
{
  time_avg=time_avg+(avg_data[w,1]/10)
  acc_avg=acc_avg+(avg_data[w,2]/10)
}
for(w in 1:10)
{
  sd2[1]=sd2[1]+(avg_data[w,1]-time_avg)*(avg_data[w,1]-time_avg)
  sd2[2]=sd2[2]+(avg_data[w,1]-acc_avg)*(avg_data[w,1]-acc_avg)
}
sd2[1]=sqrt(sd2[1]/10)
sd2[2]=sqrt(sd2[2]/10)




