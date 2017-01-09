######################
##Slawa Rokicki
##November 21, 2013
##Harvard University
##ggplot2 cheatsheet
######################


library(ggplot2)
library(gridExtra)
mtc<-mtcars

#Basic scatterplot
p1 <- ggplot(mtc, aes(x = hp, y = mpg))

#print basic plot
p1+geom_point()

#change color of points
p2 <- p1 + geom_point(color="red")            #set one color for all points
p3 <- p1 + geom_point(aes(color = wt))        #set color scale by a continuous variable
p4 <- p1 + geom_point(aes(color=factor(am)))  #set color scale by a factor variable
grid.arrange(p2, p3, p4, nrow=1)

#Change default colors in color scale
p1 + geom_point(aes(color=factor(am))) + scale_color_manual(values = c("orange", "purple"))


#change shape or size points
p2 <- p1 + geom_point(size = 5)                   #increase all points to size 5
p3 <- p1 + geom_point(aes(size = wt))             #set point size by continuous variable
p4 <- p1 + geom_point(aes(shape = factor(am)))    #set point shape by factor variable    

grid.arrange(p2, p3, p4, nrow=1)

#change default shapes
p1 + geom_point(aes(shape = factor(am))) + scale_shape_manual(values=c(0,2))


####lines, add regressoin line or abline to scatterplot

#connect points with line
p2 <- p1 + geom_point(color="blue") + geom_line()

#add regression line
p3 <- p1 + geom_point(color="red") + geom_smooth(method = "lm", se = TRUE)

#add vertical line
p4 <- p1 + geom_point() + geom_vline(xintercept = 100, color="red")

grid.arrange(p2, p3, p4, nrow=1)

#thick line plot
ggplot(mtc, aes(x = wt, y = qsec)) + geom_line(size=2, aes(color=factor(vs)))


###change x-axis and y-axis:
p2 <- ggplot(mtc, aes(x = hp, y = mpg)) + geom_point()

#label all axes at once
p3 <- p2 + labs(x="Horsepower", 
                y = "Miles per Gallon")   

#label and change font size
p4 <- p2 + theme(axis.title.x = element_text(face="bold", size=20)) + 
  labs(x="Horsepower")     

#adjust axis limits and breaks
p5 <- p2 + scale_x_continuous("Horsepower",
                              limits=c(0,400),
                              breaks=seq(0, 400, 50))   

grid.arrange(p3, p4, p5, nrow=1)


#legend options, tategorical scale
g1<-ggplot(mtc, aes(x = hp, y = mpg)) + geom_point(aes(color=factor(vs)))

g2 <- g1 + theme(legend.position=c(1,1),legend.justification=c(1,1))#move legend inside                
g3 <- g1 + theme(legend.position = "bottom")                        #move legend bottom         
g4 <- g1 + scale_color_discrete(name ="Engine", 
                                labels=c("V-engine", "Straight engine")) #change labels

grid.arrange(g2, g3, g4, nrow=1)

#legend options- continuous scale
g5<-ggplot(mtc, aes(x = hp, y = mpg)) + geom_point(size=2, aes(color = wt))
g5 + scale_color_continuous(name="Weight",
                            breaks = with(mtc, c(min(wt), mean(wt), max(wt))),
                            labels = c("Light", "Medium", "Heavy"),
                            low = "pink",
                            high = "red")

#themes

g2<- ggplot(mtc, aes(x = hp, y = mpg)) + geom_point()

#Completely clear all lines except axis lines and make background white
t1<-theme(                              
  plot.background = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  panel.border = element_blank(), 
  panel.background = element_blank(),
  axis.line = element_line(size=.4)
)

#Use theme to change axis label style
t2<-theme(                              
  axis.title.x = element_text(face="bold", color="black", size=10),
  axis.title.y = element_text(face="bold", color="black", size=10),
  plot.title = element_text(face="bold", color = "black", size=12)
)


g3 <- g2 + t1
g4 <- g2 + theme_bw()
g5 <- g2 + theme_bw() + t2 + labs(x="Horsepower", y = "Miles per Gallon", title= "MPG vs Horsepower")


grid.arrange(g2, g3, g4, g5, nrow=1)


#final graph with combinations of options
g2<- ggplot(mtc, aes(x = hp, y = mpg)) + 
  geom_point(size=2, aes(color=factor(vs), shape=factor(vs))) +
  geom_smooth(aes(color=factor(vs)),method = "lm", se = TRUE) +
  scale_color_manual(name ="Engine", 
                     labels=c("V-engine", "Straight engine"),
                     values=c("red","blue")) +
  scale_shape_manual(name ="Engine", 
                     labels=c("V-engine", "Straight engine"),
                     values=c(0,2)) +
  theme_bw() + 
  theme(                              
    axis.title.x = element_text(face="bold", color="black", size=12),
    axis.title.y = element_text(face="bold", color="black", size=12),
    plot.title = element_text(face="bold", color = "black", size=12),
    legend.position=c(1,1),
    legend.justification=c(1,1)) +
  labs(x="Horsepower", 
       y = "Miles per Gallon", 
       title= "Linear Regression (95% CI) of MPG vs Horsepower by Engine type")

g2




##add text to regression line

#method 1: easy
p3 <- p1 + geom_point(color="red") + geom_smooth(method = "lm", se = TRUE)
p3

m <- lm(mtc$mpg ~ mtc$hp)
a <- signif(coef(m)[1], digits = 2)
b <- signif(coef(m)[2], digits = 2)
textlab <- paste("y = ",b,"x + ",a, sep="")
print(textlab)

##basic ggplot with points and linear model 
p3 <- p1 + geom_point(color="red") + geom_smooth(method = "lm", se = TRUE)

##add regression text using geom_text
r1 <- p3 + geom_text(aes(x = 245, y = 30, label = textlab), color="black", size=5, parse = FALSE)
r1

##add regression text using annotate
r2 <- p3 + annotate("text", x = 245, y = 30, label = textlab, color="black", size = 5, parse=FALSE)
r2

#method2: fancy
##function to create equation expression
lm_eqn = function(x, y, df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == b %.% italic(x) + a,
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2)))
  as.character(as.expression(eq));                 
}

##add regression equation using geom_text
r3 <- p3 + geom_text(aes(x = 245, y = 30, label = lm_eqn(mtc$hp, mtc$mpg, mtc)), color="black", size=5, parse = TRUE)
r3

##add regression equation using annotate
r4 <- p3 + annotate("text", x = 245, y = 30, label = lm_eqn(mtc$hp, mtc$mpg, mtc), color="black", size = 5, parse=TRUE)
r4




