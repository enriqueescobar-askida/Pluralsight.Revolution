####################################
##Slawa Rokicki
##July 7, 2014
##Harvard University
##3 ways functions can improve R code
####################################


#create the data
set.seed(10)
bpdata<-data.frame(bp=rnorm(1000,140,20),
                   age=rnorm(1000,50,3),
                   sex=rbinom(1000,1,.5),
                   race=as.factor(c(rep(1,500),rep(2,500))),
                   out=rbinom(1000,1,.8))
bpdata[c(100,200,400),2]<-NA
bpdata[c(300),1]<-400

#1. avoid repetition

#how to calculate robust std errors
library(sandwich)
cov.fit1 <- vcovHC(fit1, type = "HC")
rob.std.err <- sqrt(diag(cov.fit1))

#turn into a function
#function to calculate robust standard errors for a model
robust.SE<-function(modelfit){
  require(sandwich, quietly=TRUE)
  cov.fit1 <- vcovHC(modelfit, type = "HC")
  rob.std.err <- sqrt(diag(cov.fit1))
  return(rob.std.err)
}

#get robust SEs for model 1
model1<-lm(out~age + sex, data=bpdata)
robust.SE(model1)

#get robust SEs for model2
model2<-lm(bp~age+sex, data=bpdata)
robust.SE(model2)

#2. customize output

#use the basic summary built-in function
summary(bpdata)

#function to summarize the variables in the data
summarize.vars<-function(data){
  
  #use dummies package to turn all factors into dummies
  require(dummies, quietly=TRUE)
  dat.d<-dummy.data.frame(data, dummy.class="factor")
  
  #use apply to calculate statistics for each variable
  mat<-t(apply(dat.d, 2, function(x) c(length(x), 
                                       round(mean(x, na.rm=TRUE),2), 
                                       round(sd(x, na.rm=TRUE),2), 
                                       round(min(x, na.rm=TRUE),2), 
                                       round(max(x, na.rm=TRUE),2), 
                                       length(x)-length(x[!is.na(x)]))))
  
  #assign column names and rownames to output table
  colnames(mat)<-c("N","Mean","SD","Min","Max","Num Missing")
  rownames(mat)<-colnames(dat.d)
  return(mat)
}

summarize.vars(bpdata)

#only for race 1
summarize.vars(bpdata[bpdata$race==1,])

#linear model function
lm.model.diagnostics<-function(formula, dataset){
  
  #run model and print specific output
  model1<-lm(formula=formula, data=dataset)
  stats<-round(c(summary(model1)$fstatistic[c(1,3)], 
                 summary(model1)$sigma, 
                 summary(model1)$r.squared, 
                 summary(model1)$adj.r.squared),3)
  names(stats)<-c("F","DF", "Sigma","Rsq","AdjRsq")
  l1<-list(round(summary(model1)$coefficients,3), stats)
  names(l1)<-c("Coefficients","Stats")
  print(l1)
  
  #run specific diagnostic tests
  par(mfrow=c(1,3))
  hist(model1$residuals, main="Histogram of residuals", xlab="")
  plot(model1, 1)
  plot(model1, 2)
}

#run function for model of blood pressure on age and sex
lm.model.diagnostics(bp~age+sex, bpdata)

#take out the outlier
lm.model.diagnostics(bp~age+sex, bpdata[c(-300),])

#save formula in an object 
form1<-bp~age+sex
lm.model.diagnostics(form1, bpdata)

#3. functions to improve analysis

#function to get confidence intervals for glm output, can get exponentiated output for logit or poisson
glmCI <- function(glmfit, exponent=FALSE, alpha=0.05, digits=2)
{
  #get SE from model fit
  se <- sqrt(diag(summary(glmfit)$cov.scaled))
  
  #calculuate CI for linear case
  mat <- cbind(coef(glmfit),
               coef(glmfit) - qnorm(1-alpha/2)*se,
               coef(glmfit) + qnorm(1-alpha/2)*se)
  colnames(mat) <- c("Beta", "LowerCI", "UpperCI")
  
  #if exponent=TRUE, exponeniate the coefficients and CIs
  if(exponent == TRUE)
  {
    mat <- exp(mat)
    if(summary(glmfit)$family$link=="logit") colnames(mat)[1] <- "OR"
    if(summary(glmfit)$family$link=="log") colnames(mat)[1] <- "IRR"
  }
  
  #return a rounded matrix of results
  return(round(mat, digits=digits))
}

#1. use glm with identity link on continuous response data (default family is gaussian)
g.glm<-glm(bp~age+sex, data=bpdata)
glmCI(g.glm)

#2. use glm with logit link on binary response data
b.glm<-glm(out~age+sex+bp, family=binomial(link="logit"), data=bpdata)
glmCI(b.glm, exponent=TRUE, digits=3)

