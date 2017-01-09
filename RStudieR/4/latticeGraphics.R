library(ggplot2) # Using this for the diamonds dataset

library(lattice)
demo('lattice',package='lattice')

barchart(diamonds$color)
xyplot(price ~ carat, data = diamonds)

#Comparing ggplot2 and lattice
#http://learnr.wordpress.com/2009/06/28/ggplot2-version-of-figures-in-lattice-multivariate-data-visualization-with-r-part-1/