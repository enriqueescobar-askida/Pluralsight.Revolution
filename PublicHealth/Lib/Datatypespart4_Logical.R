
###R code for: Data types part 4: Logical class
###November 2012
###Slawa Rokicki srokicki@fas.harvard.edu

##set working directory and load data (Rdata format)
setwd("insert_pathname_here")
load("mydata.Rdata")
View(mydata)

##creating logical objects
mydata$Age<25
mydata$Ageunder25<-mydata$Age<25
class(mydata$Ageunder25)

##subsetting using logical class
mysubset<-mydata[mydata$Age<40,]

##ifelse statement to convert continuous to binary
mydata$Young<-ifelse(mydata$Age<25,1,0)

##convert the logical into numeric - both of the statements below do the exact same thing
mydata$Ageunder25_num<-as.numeric(mydata$Ageunder25)
mydata$Ageunder25_num<-as.numeric(mydata$Age<25)
View(mydata[,c(1,2,8,10)])

##now use logical class to your advantage

##Create a height vector that includes both inches and cm
mydata$Height_wrong<-c(60, 152, 67, 170, 64, NA, 67, 65, NA, 60)
View(mydata[,c(1,11)])

##Create new variable that keeps the values that are in inches, but converts the values in cm to inches 
mydata$Height_fixed_in<-as.numeric(mydata$Height_wrong<90)*mydata$Height_wrong + as.numeric(mydata$Height_wrong>=90)*mydata$Height_wrong/2.54

View(mydata[,c(1,11,12)])


##Create new dataframe with ID, Age of last child, number of other children
newdata<-data.frame(cbind(ID=1:10,Child1age=c(2,3,NA,12,2,5,NA,NA,6,6), Numotherchildren=c(0,1,NA,3,0,0,NA,NA,0,1)))

View(newdata)

##Show whether values of Child1age are NA or not
is.na(newdata$Child1age)

##Show whether values of Child1age are NOT NA 
!is.na(newdata$Child1age)

##Create new variable that counts total number of children by adding 1 from the Child1age vector if the value is not missing, plus the number of the other children in the family
newdata$Totalnumchildren<-as.numeric(!is.na(newdata$Child1age))+newdata$Numotherchildren

##Replace those values of Totalnumberchildren that are missing to 0
newdata$Totalnumchildren[is.na(newdata$Totalnumchildren)]<-0







