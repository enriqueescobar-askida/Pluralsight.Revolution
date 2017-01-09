##Code for: Data types part 2: Using classes to your advantage
##November 2012
###Slawa Rokicki srokicki@fas.harvard.edu

##set the working directory and read in the data
setwd("insert_pathname_here")
mydata<-read.csv("mydata_datatypespart2.csv")


##check out the classes of variables in the dataframe
ls.str()
class(mydata$Height)
class(mydata$Weight)

##create BMI variable (numeric vector in my dataframe)
mydata$BMI<-mydata$Weight/(mydata$Height)^2 * 703
View(mydata)

##summary statistics of numeric variable
summary(mydata$Age)

#just the minimum
summary(mydata$Age)[1]

##create summary matrix of three variables
summary.matrix<-rbind(summary(mydata$Age)[c(1,4,6)], summary(mydata$BMI)[c(1,4,6)], summary(mydata$Sex)[c(1,4,6)])
rownames(summary.matrix)<-c("Age", "BMI", "Sex")
class(summary.matrix)
summary.matrix

##Do this more efficiently with apply function
summary.matrix2<-apply(mydata[,c(2,3,7)], 2, FUN=function(x) c(mean(x,na.rm=TRUE), sd(x, na.rm=TRUE)))
rownames(summary.matrix2)<-c("Mean", "Stdev")
round(summary.matrix2, 2)

##use classes to your advantage when plotting

##Assign numeric vector for the range of x-axis
agelimit<-c(20,80)

##Assign numeric single scalar to plotsymbols and meanage
plotsymbols<-2
meanage<-mean(mydata$Age)

##Assign single character words to plottype and plotcolor 
plottype<-"p"
plotcolor<-"darkgreen"

##Assign a vector of characters to titletext
titletext<-c("Scatterplot", "vs Age")

##plot area is 1 row, 3 columns
par(mfrow=c(1,3))

##plot all three plots using the assigned objects

plot(mydata$Age, mydata$Height, xlab="Age", ylab="Height", xlim=agelimit, pch=plotsymbols, type=plottype,col=plotcolor, main=paste(titletext[1], "Height", titletext[2]))
abline(v=meanage)

plot(mydata$Age, mydata$Weight, xlab="Age", ylab="Weight", xlim=agelimit,pch=plotsymbols, type=plottype,col=plotcolor, main=paste(titletext[1], "Weight", titletext[2]))
abline(v=meanage)

plot(mydata$Age, mydata$BMI, xlab="Age", ylab="BMI", xlim=agelimit,pch=plotsymbols, type=plottype, col=plotcolor,main=paste(titletext[1], "BMI", titletext[2]))
abline(v=meanage)





