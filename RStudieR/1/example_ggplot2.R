library(ggplot2)
head(diamonds)
ggplot(diamonds, aes(color, fill=cut)) + geom_bar()