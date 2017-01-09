
###R code for: Data types part 3: Factors!
###November 2012
###Slawa Rokicki srokicki@fas.harvard.edu

##set working directory and load data (Rdata format)
setwd("insert_pathname_here")
load("mydata.Rdata")

View(mydata)

##check out what kinds of variables there are
ls.str(mydata)

##investigate the factor variable Race
levels(mydata$Race)
summary(mydata$Race)

plot(mydata$Age~mydata$Race, xlab="Race", ylab="Age", main="Boxplots of Age by Race")

##regression of age on race

##first black is the reference
summary(lm(Age~Race, data=mydata))

##then I relevel and Hispanic is the reference
mydata$Race2<-relevel(mydata$Race, "Hispanic")
summary(lm(Age~Race2, data=mydata))


###create unordered and ordered factors
mydata$Married.cat<-factor(mydata$Married, labels=c("Single", "Married", "Divorced/Widowed"))
class(mydata$Married.cat)
levels(mydata$Married.cat)

mydata$Weight.type<-ordered(cut(mydata$Weight, c(0,135,165,200), labels=c("Low", "Medium", "High")))                    
class(mydata$Weight.type)
levels(mydata$Weight.type)

##check out how those new variables look
View(mydata)

###fixing the problems of leftover factor categories 
##- subset data and Hispanic is no longer there but the category remains
mynewdata<-mydata[1:6,]
summary(mynewdata$Race)

##use droplevels to drop all unused levels
finaldata<-droplevels(mynewdata)
summary(finaldata$Race)

##or do it all in one go without changing name of the dataset
mydata<-droplevels(mydata[1:6,])
levels(mydata$Race)
View(mydata)
