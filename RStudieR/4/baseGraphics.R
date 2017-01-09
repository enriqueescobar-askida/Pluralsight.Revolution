library(ggplot2)

# Base Graphics package
help(graphics)
demo(graphics, package='graphics')
plot(diamonds$color) 

colors <- table(diamonds$color)
pie(colors, labels=names(colors))
dotchart(colors)

boxplot(which="plot", diamonds$carat)
plot(diamonds$color, diamonds$carat)
plot(diamonds$carat, diamonds$color)

hist(diamonds$carat)
stripchart(diamonds$carat)

spineplot(diamonds$carat, diamonds$color)
pairs(data=diamonds, carat ~ price)

# Three examples to compare with other packages
plot(x=diamonds$cut)  # bar
plot(x=diamonds$color, y=diamonds$price) # box
plot(x=diamonds$x, y=diamonds$y, cex=.3) # scatter
