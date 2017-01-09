demo(package='ggplot2') #None

# Comparing base R plots and ggplot2

# Bar Chart
plot(x=diamonds$color) 
ggplot(diamonds, aes(x=color) ) + geom_bar()

# Box Chart
plot(x=diamonds$color, y=diamonds$price)
ggplot(diamonds, aes(x=color, y=price)) + geom_boxplot()

# scatter plot
plot(x=diamonds$x, y=diamonds$y, cex=.3)
ggplot(diamonds, aes(x=x, y=y)) + geom_point()


# ggvis
library(ggvis)
library(ggplot2)
ggplot(diamonds, aes(x=x, y=y))  +  geom_point()
ggvis (diamonds,      ~x,   ~y) %>% layer_points()
ggvis (diamonds,      ~x,   ~y) %>% layer_points(size := input_slider(10, 300, value = 10))