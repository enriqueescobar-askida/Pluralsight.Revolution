
##Slawa Rokicki
##List and apply()

#create a list
mylist<-list(x=c(1,5,7), y=c(4,2,6), z=c(0,3,4))
mylist

#use lapply to calculate mean of each element of list
lapply(mylist, function(x) mean(x))

#use sapply instead to do same thing, but to return a vector
sapply(mylist, function(x) mean(x))

#user-written function with sapply
#write function to find the span of the numbers in a vector and check if the span is larger than 5
span.fn<-function(x) {(max(x)-min(x))>=5}

#apply that function to the list
sapply(mylist, span.fn)

#initialize list with lapply
#initialize list to to 2 empty matrices of 2 by 3
list2<-lapply(1:2, function(x) matrix(NA, nrow=2, ncol=3))
list2

#initialize list to 2 matrices with random numbers from standard normal distribution
list2<-lapply(1:2, function(x) matrix(rnorm(6, 10, 1), nrow=2, ncol=3))
list2

#manipulate this list
#input list, output column sums of each matrix into a new list
lapply(list2, colSums)

#input list, output column sums of each component into a **vector** (which binds them into a matrix)
sapply(list2, colSums)

#instead of binding, we can stack these column sums by using tranpose function t():
t(sapply(list2, colSums))

#use lists when running models
#create some data
set.seed(2000)
x=rbinom(1000,1,.6)
mydata<-data.frame(trt=x,
                   out1=x*3+rnorm(1000,0,3),
                   out2=x*5+rnorm(1000,0,3),
                   out3=rnorm(1000,5,3),
                   out4=x*1+rnorm(1000,0,8))

head(mydata)

#initialize my results vector
results<-vector("list", 4) 

#first way - use a loop
for(i in 1:4){
  results[[i]]<-lm(mydata[,i+1]~trt, data = mydata) 
}


#secod way - use lapply for an even more amazing way to do this!
results<-lapply(2:5, function(x) lm(mydata[,x]~trt, data = mydata))
names(results)<-names(mydata)[2:5]

#print out trt effect:
sapply(results, function(x) summary(x)$coefficients[2,1:2])

require(stargazer)
stargazer(results, 
          column.labels=names(results),
          keep.stat=c("rsq","n"),
          dep.var.labels="")

#plot it
#extract coefficients from the list
coefs<-as.data.frame(t(sapply(results, function(x) summary(x)$coefficients[2,1:2])))
coefs

#add outcome columnn and change name of SE column
coefs$Outcome<-rownames(coefs)
names(coefs)[2]<-"SE"

#use ggplot to plot all the estimates
require(ggplot2)
ggplot(coefs, aes(Outcome,Estimate)) +
  geom_point(size=4) + 
  theme(legend.position="none")+
  labs(title="Treatment effect on outcomes", x="", y="Estimate and 95% CI")+
  geom_errorbar(aes(ymin=Estimate-1.96*SE,ymax=Estimate+1.96*SE),width=0.1)+
  geom_hline(yintercept = 0, color="red")+
  coord_flip()


