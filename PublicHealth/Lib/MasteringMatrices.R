###R code for: Mastering Matrices
###April 2013
###Slawa Rokicki srokicki@fas.harvard.edu

##read in Rdata
load("mydata.Rdata")

##converting from dataframe to matrix
mydata.mat<-as.matrix(mydata)
mydata.mat

####dropping columns and converting so matrix is numeric
mydata.mat<-as.matrix(mydata[,1:6])
mydata.mat<-as.matrix(mydata[,-7])

#same for dataframes and matrices
mydata[,-3]
mydata.mat[,-3]

####adding column

#to dataframe
mydata$agesq<-mydata$Age^2

#to matrix
mydata.mat<-cbind(mydata.mat, mydata.mat[,2]^2)
mydata.mat<-cbind(mydata.mat, mydata.mat[,"Age"]^2)

#renaming columns
colnames(mydata.mat)[7:8]<-c("AgeSq", "AgeSqAgain")


##show how matrices can be helpful in storing values
mat1<-matrix(NA, nrow=1000, ncol=3)
for(i in 1:nrow(mat1)){
  
  vec1<-rpois(1,1)
  vec2<-rpois(10,1)
  vec3<-rpois(100,1)
  mat1[i,]<-c(mean(vec1), mean(vec2), mean(vec3))
  
}

##create the histograms
par(mfrow=c(1,3))
hist(mat1[,1], main="n=1", xlab="Mean")
hist(mat1[,2], main="n=10", xlab="Mean")
hist(mat1[,3], main="n=100", xlab="Mean")












