
###R code for: Data types part 1: Ways to store variables
###November 2012
###Slawa Rokicki srokicki@fas.harvard.edu

##Create various objects of different classes

#scalar
numeric.var<-10
character.var<-"Hello!"

#vector
vector.numeric<-c(1,2,3,10)
vector.char<-rep("abc",3)

#matrix
matrix.numeric<-matrix(data=c(1:6),nrow=3,ncol=2)
matrix.character<-matrix(data=c("a","b","c","d"), nrow=2, ncol=2)


##dataframe
dataframe.var<-data.frame(cbind(School=1, ID=1:5, Test=c("math","read","math","geo","hist")))
class(dataframe.var)
class(dataframe.var$ID)
dataframe.var$ID<-as.numeric(dataframe.var$ID)

##notice that any data read in using read.csv or other read functions are always dataframes
dataframe.var2<-read.csv(file="mydata.csv")
class(dataframe.var2)

##list
list.var<-list(numeric.var, vector.char, matrix.numeric, dataframe.var)
class(list.var)

##list all the variables
ls()

##print all variables
numeric.var
character.var

vector.numeric
vector.char

matrix.numeric
matrix.character

dataframe.var
dataframe.var2

list.var

##remove one objects
rm(character.var)

##remove all objects
rm(list=ls())
ls()
