###R code for: Translating Weird R Errors
###January 2013
###Slawa Rokicki srokicki@fas.harvard.edu


##make some data
prob1<-as.data.frame(cbind(c(1,2,3),c(5,4,3)))
colnames(prob1)<-c("Education","Ethnicity")


######1. The variable you're referecing doesn't exist:
##incorrect - missing an "a"!
table(prob1$Eduction, prob1$Ethnicity)

##correct
table(prob1$Education, prob1$Ethnicity)


######2. Similar problem, different error message
##incorrect - dont' have Sex in the dataset
prob1$Sex_recode<-as.numeric(prob1$Sex==2)

##correct
prob1$Educ_recode<-as.numeric(prob1$Education==2)



######3. Undefined columns - I forgot a comma
##incorrect - need to define both which rows we want, and which columns we want
nrow(prob1[prob1$Education!=1])

#correct
nrow(prob1[prob1$Education!=1,])




