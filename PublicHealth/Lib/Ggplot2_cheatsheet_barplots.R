######################
##Slawa Rokicki
##January 5, 2014
##Harvard University
##ggplot2 cheatsheet - barcharts
######################


library(ggplot2)
library(gridExtra)
mtc<-mtcars
head(mtc)

##bar charts

#basic bar chart
ggplot(mtc, aes(x = factor(gear))) + geom_bar(stat="bin")

#useful: http://docs.ggplot2.org/0.9.3.1/geom_bar.html
#useful: http://www.cookbook-r.com/Graphs/

#using aggregate
ag.mtc<-aggregate(mtc$wt, by=list(mtc$gear), FUN=mean)
ag.mtc

#using tapply
summary.mtc <- data.frame(
  gear=levels(as.factor(mtc$gear)),
  meanwt=tapply(mtc$wt, mtc$gear, mean))

summary.mtc

#basic bar plot for aggregated data
#using aggregated dataset
ggplot(summary.mtc, aes(x = factor(gear), y = meanwt)) + geom_bar(stat = "identity")

#using original dataset
ggplot(mtc,aes(x=factor(gear), y=wt)) + stat_summary(fun.y=mean, geom="bar")


#change look of barplot

#1. horizontal bars
ggplot(mtc,aes(x=factor(gear),y=wt)) + stat_summary(fun.y=mean,geom="bar") +
  coord_flip()

#2. change colors of bars
ggplot(mtc,aes(x=factor(gear),y=wt,fill=factor(gear))) +  stat_summary(fun.y=mean,geom="bar") +
  scale_fill_manual(values=c("purple", "blue", "darkgreen"))

#3. change width of bars
ggplot(mtc,aes(x=factor(gear),y=wt)) +  stat_summary(fun.y=mean,geom="bar", aes(width=0.5))
ggplot(summary.mtc, aes(x = factor(gear), y = meanwt)) + geom_bar(stat = "identity", width=0.2)

#by another variable
#1. next to each other
ggplot(mtc,aes(x=factor(gear),y=wt,fill=factor(vs)), color=factor(vs)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")

#2. stacked
ggplot(mtc,aes(x=factor(gear),y=wt,fill=factor(vs)), color=factor(vs)) +  
  stat_summary(fun.y=mean,position="stack",geom="bar")

#reorder stacking
mtc$vs2<-factor(mtc$vs, levels = c(1,0))

ggplot(mtc,aes(x=factor(gear),y=wt,fill=factor(vs2)), color=factor(vs2)) +  
  stat_summary(fun.y=mean,position="stack",geom="bar")

#3. with facets
ggplot(mtc,aes(x=factor(gear),y=wt,fill=factor(vs)), color=factor(vs)) +  
  stat_summary(fun.y=mean, geom="bar") +
  facet_wrap(~vs)

#add text to bars
ag.mtc<-aggregate(mtc$wt, by=list(mtc$gear,mtc$vs), FUN=mean)
colnames(ag.mtc)<-c("gear","vs","meanwt")
ag.mtc

#1. basic
ggplot(ag.mtc, aes(x = factor(gear), y = meanwt, fill=factor(vs),color=factor(vs))) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(y=meanwt, ymax=meanwt, label=meanwt),position= position_dodge(width=0.9), vjust=-.5)

#2. fixing the yaxis problem, changing the color of text, legend labels, and rounding to 2 decimals
ggplot(ag.mtc, aes(x = factor(gear), y = meanwt, fill=factor(vs))) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(y=meanwt, ymax=meanwt, label=round(meanwt,2)), position= position_dodge(width=0.9), vjust=-.5, color="black") +
  scale_y_continuous("Mean Weight",limits=c(0,4.5),breaks=seq(0, 4.5, .5)) + 
  scale_x_discrete("Number of Gears") +
  scale_fill_discrete(name ="Engine", labels=c("V-engine", "Straight engine"))

#add error bars
summary.mtc2 <- data.frame(
  gear=levels(as.factor(mtc$gear)),
  meanwt=tapply(mtc$wt, mtc$gear, mean),
  sd=tapply(mtc$wt, mtc$gear, sd))
summary.mtc2
ggplot(summary.mtc2, aes(x = factor(gear), y = meanwt)) + 
  geom_bar(stat = "identity", position="dodge", fill="lightblue") +
  geom_errorbar(aes(ymin=meanwt-sd, ymax=meanwt+sd), width=.3, color="darkblue")

#add linear line
#summarize data
summary.mtc3 <- data.frame(
  hp=levels(as.factor(mtc$hp)),
  meanmpg=tapply(mtc$mpg, mtc$hp, mean))

#run a model
l<-summary(lm(meanmpg~as.numeric(hp), data=summary.mtc3))

#manually entering the intercept and slope
f1<-ggplot(summary.mtc3, aes(x = factor(hp), y = meanmpg)) + 
  geom_bar(stat = "identity",  fill="darkblue")+
  geom_abline(aes(intercept=l$coef[1,1], slope=l$coef[2,1]), color="red", size=1.5)

#using stat_smooth to fit the line for you
f2<-ggplot(summary.mtc3, aes(x = factor(hp), y = meanmpg)) + 
  geom_bar(stat = "identity",  fill="darkblue")+
  stat_smooth(aes(group=1),method="lm", se=FALSE, color="orange", size=1.5)


