####################################
##Slawa Rokicki
##October 21, 2014
##Harvard University
##Easy Clustered Standard Errors in R
####################################

#set up
rm(list = ls())

library(plm)
library(lmtest)
data(Crime)

#basic linear model with standard variance estimate
Crime$region<-factor(Crime$region)
m1<-lm(crmrte~pctymle+polpc+region+year, data=Crime)

#write your own function to return variance covariance matrix under clustered SEs
get_CL_vcov<-function(model, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  
  #calculate degree of freedom adjustment
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  
  #calculate the uj's
  uj  <- apply(estfun(model),2, function(x) tapply(x, cluster, sum))
  
  #use sandwich to get the var-covar matrix
  vcovCL <- dfc*sandwich(model, meat=crossprod(uj)/N)
  return(vcovCL)
}

#call our new function and save the var-cov matrix output in an object
m1.vcovCL<-get_CL_vcov(m1, Crime$county)

#equivalent way: use the cluster.vcov function to get variance-covariance matrix
library(multiwayvcov)
m1.vcovCL.2<-cluster.vcov(m1, Crime$county)

#show difference between regular SEs and clustered SEs
#the regular OLS standard errors
coeftest(m1)

#the clustered standard errors by indicating the correct var-covar matrix
coeftest(m1, m1.vcovCL)

##confidence intervals and F-tests

#function to return confidence intervals
get_confint<-function(model, vcovCL){
  t<-qt(.975, model$df.residual)
  ct<-coeftest(model, vcovCL)
  est<-cbind(ct[,1], ct[,1]-t*ct[,2], ct[,1]+t*ct[,2])
  colnames(est)<-c("Estimate","LowerCI","UpperCI")
  return(est)
}

#do a wald test to get F-statistic
waldtest(m1, vcov = m1.vcovCL, test = "F")

#obtain the confidence interval using our function
get_confint(m1, m1.vcovCL)


#one function to everything - input your model and the clustering variable, output coefficients/SEs, F-test, and CIs
super.cluster.fun<-function(model, cluster)
{
  require(multiwayvcov)
  require(lmtest)
  vcovCL<-cluster.vcov(model, cluster)
  
  coef<-coeftest(model, vcovCL)
  w<-waldtest(model, vcov = vcovCL, test = "F")
  ci<-get_confint(model, vcovCL)
  
  return(list(coef, w, ci))
}

super.cluster.fun(m1, Crime$county)

#save waldtest in an object
w<-waldtest(m1, vcov = m1.vcovCL, test = "F")

#check out what is saved in w
names(w)

#use $F to get the F-statistic
w$F[2]

#r-squared
summary(m1)$r.squared

#hypothesis testing
library(car)
linearHypothesis(m1, vcov=m1.vcovCL, "regionwest = regioncentral")

#troubleshooting with the user-written function
#error messages
get_CL_vcov(m1, Crime$couunty)

#force in missing value in outcome
Crime2<-Crime
Crime2$crmrte[1]<-NA

#rerun model
m2<-lm(crmrte~pctymle+polpc+region+year, data=Crime2)

#this produces errors 
v1<-get_CL_vcov(m2, Crime2$county)

#but we can remove the observations in county for which crmrte is missing 
v2<-get_CL_vcov(m2, Crime2$county[!is.na(Crime2$crmrte)])

#or can use na.omit
Crime3<-na.omit(Crime2)
m3<-lm(crmrte~pctymle+polpc+region+year, data=Crime3)
v3<-get_CL_vcov(m3, Crime3$county)

#using plm to do clustered standard errors
#use plm function to define formula, dataset, that it's a pooling model, and the cluster variable
p1 <- plm(crmrte~pctymle+polpc+region+year, Crime, model='pooling', index=c('county'))

#calculate small sample size adjustment
G <- length(unique(Crime$county))
N <- length(Crime$county)
dfa <- (G/(G - 1)) * (N - 1)/p1$df.residual

#use coeftest and the vcovHC functions, specifying HC0 type and identifying cluster as 'group'
coeftest(p1, vcov=function(x) dfa*vcovHC(x, cluster="group", type="HC0"))



