library(ggplot2)
getwd()
setwd('~/R_images')

png('diamonds_color.png')
plot(diamonds$color)
dev.off()

png('diamonds_color2.png')
ggplot(diamonds, aes(x=color)) + geom_bar()
dev.off()

ggplot(diamonds, aes(x=color, fill=color)) + geom_bar()
ggsave('diamonds_color3.png')