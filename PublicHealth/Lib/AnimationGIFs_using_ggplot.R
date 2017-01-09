

#animation plot

rm(list = ls())

library(animation)
library(ggplot2)
library(colorspace)

setwd("~/Dropbox/Harvard Doctoral/Rforpublichealth/post28")

# 1. Tracing a regression line

#make up some data
tracedat<-data.frame(x=rnorm(1000,0,1))
tracedat$y<-abs(tracedat$x)*2+rnorm(1000,0,3)

#predict a spline fit and add predicted values to the dataframe
loess_fit <- loess(y ~ x, tracedat)
tracedat$predict_y<-predict(loess_fit)

#plot finished scatterplot with loess fit
ggplot(tracedat, aes(x,y)) +
  geom_point() +
  geom_line(data=tracedat, aes(x,predict_y), color="red", size=1.3) + 
  scale_x_continuous(limits=c(-3, 3)) + 
  scale_y_continuous(limits=c(-10, 10))

#function to draw the scatterplot, but the curve fit only up to whatever index we set it at
draw.curve<-function(cutoff){
  a<-ggplot(tracedat, aes(x,y)) +
    geom_point() +
    geom_line(data=tracedat[tracedat$x<cutoff,], aes(x,predict_y), color="red", size=1.3) + 
    scale_x_continuous(limits=c(-3, 3)) + 
    scale_y_continuous(limits=c(-10, 10))
  
  print(a)
}

#try it out: draw curve up to cutoff x-value of -2
draw.curve(cutoff=-2)

#function to iterate over the full span of x-values
trace.animate <- function() {
  lapply(seq(-3,3,.2), function(i) {
    draw.curve(i)
  })
}

#save all iterations into one GIF
saveGIF(trace.animate(), interval = .2, movie.name="trace_3.gif")

#2. Diverging density plots

dist.data<-data.frame(base=rnorm(5000, 0, 3), 
                      follow=rnorm(5000, 0, 3), 
                      type=c(rep("Type 1",2500),rep("Type 2",2500)))
dist.data$follow<-ifelse(dist.data$type=="Type 2", dist.data$follow+12, dist.data$follow)

#plot one to make sure it's working. Use aes_string rather than aes
p<-ggplot(dist.data, aes_string("base", fill="type")) +
  geom_density(alpha=0.5) +
  theme(legend.position="bottom") +
  scale_x_continuous(limits=c(-10, 20)) + 
  scale_y_continuous(limits=c(0, 0.20)) +
  scale_fill_manual("", labels=c("Type 1", "Type 2"), values = c("orange","purple")) +
  labs(x="Score",y="Density", title="title")

#function that plots a density plot with arguments for the variable to plot and the title
plot.dens<-function(plot.item, title.item){
  p<-ggplot(dist.data, aes_string(plot.item, fill="type"))+
    geom_density(alpha=0.5) +
    theme(legend.position="bottom") +
    scale_x_continuous(limits=c(-10, 20)) + 
    scale_y_continuous(limits=c(0, 0.20)) +
    scale_fill_manual("", labels=c("Type 1", "Type 2"), values = c("orange","purple"))+
    labs(x="Score",y="Density", title=title.item)
  
  print(p)
}

#try it out - plot it for the follow data with the title "After Intervention"
plot.dens(plot.item="follow", title.item="After Intervention")

#function that iterates over the two different plots
distdiverge.animate <- function() {
  items<-c("base", "follow")
  titles<-c("Before Intervention","After Intervention")
  lapply(seq(1:2), function(i) {
    plot.dens(items[i], titles[i])
  })
}

#save in a GIF
saveGIF(distdiverge.animate(), interval = .65, movie.name="dist3.gif")

#3. Happy New Year plot

#create dataset
happy2015<-data.frame(x=rnorm(500, 0, 1.5), y=rnorm(500, 0, 1.5), z=rnorm(500,0,1.5))

#create objects to hold the letters, colors, and x and y coordinates that we will scroll through
sign<-c("H","A","P","P","Y","2","0","1","5","!!")
colors <- rainbow_hcl(10, c=300)
xcoord<-rep(c(-2, -1, 0, 1, 2),2)
ycoord<-c(2, 1.7, 2.1, 1.5, 2, -.5, 0, -1, -.8, -.7)

#set up the theme in an object (get rid of axes, grids, and legend)
theme.both<- theme(legend.position="none", 
                   panel.background = element_blank(),
                   axis.ticks = element_blank(),
                   axis.line = element_blank(), 
                   axis.text.x = element_blank(), 
                   axis.text.y = element_blank(),
                   plot.background = element_rect(fill = "black"),
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank())

#plot the first letter (set index=1 to get the first element of color, letter, and coordinates)
index<-1
ggplot(happy2015, aes(x, y, alpha = z, color=z)) + 
  geom_point(alpha=0.2) + labs(title="", x="", y="") + 
  theme.both + 
  scale_colour_gradient(low = "white", high="lightblue")+
  annotate("text", x=xcoord[index], y=ycoord[index], size=15, label=sign[index], color=colors[index])

#set up function to create a new dataset, plot it, and annotate it by an index argument
draw.a.plot<-  function(index){
  
  #make up a new dataframe
  happy2015<-data.frame(x=rnorm(500, 0, 1.5), y=rnorm(500, 0, 1.5), z=rnorm(500,0,1.5))
  
  #plot according to the index passed
  g<-ggplot(happy2015, aes(x, y, alpha = z, color=z)) + 
    geom_point(alpha=0.2) + labs(title="", x="", y="") + 
    theme.both + 
    scale_colour_gradient(low = "white", high="lightblue")+
    annotate("text", x=xcoord[index], y=ycoord[index], size=15, label=sign[index], color=colors[index])
  
  #print out the plot
  print(g)
}

#set up function to loop through the draw.a.plot() function
loop.animate <- function() {
  lapply(1:length(sign), function(i) {
    draw.a.plot(i)
  })
}

#save the images into a GIF
saveGIF(loop.animate(), interval = .5, movie.name="happy2015-2.gif")





