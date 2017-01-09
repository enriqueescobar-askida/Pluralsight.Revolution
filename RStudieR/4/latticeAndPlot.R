# Compare lattice and plot from base grapics

plot(iris$Sepal.Length, iris$Sepal.Width, cex=0.5)

# plot(x, y)
# xyplot(y ~ x, data = dataset)

# Lattice uses xyplot y ~ x
# The dataframe is split out, so only the columns need to be identified.

xyplot(Sepal.Width ~ Sepal.Length, iris)

# Lattice is better at dealing with additional variables.

# At times, analyzing 2 variables is insufficient.
# Interaction with a third variable might affect the relationship.

# This is possible but gets a bit cumbersome in base graphics.


par(mfrow=c(2,2))
s<-subset(iris, Species=='virginica')
plot(s$Sepal.Length, s$Sepal.Width, cex=0.5)
title(main = "virginica")

# skip a cell.
plot.new()

s<-subset(iris, Species=='setosa')
plot(s$Sepal.Length, s$Sepal.Width, cex=0.5)
title(main = "setosa")
s<-subset(iris, Species=='versicolor')
plot(s$Sepal.Length, s$Sepal.Width, cex=0.5)
title(main = "versicolor")

# Base graphics often requires a number of short function calls
# lattice charts can often be created with a single long call

xyplot(Sepal.Width ~ Sepal.Length | Species, iris)

# y~x | A means display the relationship between variables y and x
# separately for every combination of factor A