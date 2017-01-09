
# We start with a bar chart that indicates a count of
# the number of times each color appears in the diamonds
# data set.  You might want to make a mental note, color G has the most
# and color J the least.
ggplot(diamonds, aes(x=color) ) + geom_bar()

# The data in this example is the diamonds dataset, specifically
# a count of occurences of each value in the color field.  Data
# is one aspect of the grammar of graphics.  The count of each
# value in color is represented by the height of the bar in the 
# bar chart.  This is called an "aesthetic mapping" in ggplot2.
# We can map this data in many different ways.  For instance, in
# addition to making the size of the bar dependent upon the count
# for each value, we could indicate that - rather than dark
# grey - each bar should be its own color.  We can accomplish this
# by mapping diamond color to the fill aesthetic.

ggplot(diamonds, aes(x=color, fill=color) ) + geom_bar()

# Now each bar is its own color, and a legend is included on the side.
# One of the less obvious grammatical elements in this math is a
# coordinate system.  We are using traditional cartesian coordinate
# system where a two dimensional plain includes a horizontal axis 
# and A vertical axis.  But other coordinate systems do exist and
# are made evident when considering charts represented by the grammar
# of graphics.  So let's say we wanted to simply stack all of these
# bars into one single bar.  We can do this by assigning a single
# value to the x-axis.

ggplot(diamonds, aes(x='', fill=color) ) + geom_bar(width = 1)

# This type of chart might not seem very familiar in a cartesian
# coordinate system.  But if we represent the same graph with polar
# coordinates:
ggplot(diamonds, aes(x='', fill=color) ) + geom_bar(width = 1) + coord_polar()

# Now rather than bars, the count of each color is represented by the
# width of the ring.  By modifying the call to use polar coordinates
# to divide the circle by making the count represent the width of an
# angle emminating from the center of the circle...
ggplot(diamonds, aes(x='', fill=color) ) + geom_bar(width = 1) + coord_polar(theta="y")

# We see a familar pie chart.  

