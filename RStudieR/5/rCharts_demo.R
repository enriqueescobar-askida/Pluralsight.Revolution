require(devtools)
install_github('rCharts', 'ramnathv')

library(rCharts)

library(ggplot2)

rPlot(price ~ carat | cut, data=diamonds, color = 'cut', type='point')