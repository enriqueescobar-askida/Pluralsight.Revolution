##
## quantmod
##
library(quantmod)

portfolio<-c('GOOG','AAPL')
loadSymbols(portfolio)

barChart(AAPL)
barChart(GOOG)

# Quantmod includes functions to add technical indicators to charts
addBBands() 

## Quantmod objects use xts and zoo for time series data
class(AAPL)

## Date Ranges
AAPL['2014-10-01::2014-10-17']
barChart(AAPL['2014-10-01::2014-10-17'])
candleChart(AAPL['2014-10-01::2014-10-17'])

##
## Demo of libridate
##
library(lubridate)
wday('2014-10-18')
wday('2014-10-18', label=TRUE)

myDay<-ymd(20141018)
wday(myDay - years(1), label=TRUE)

##
## Lubridate and quantmod together
##
library(quantmod)
library(lubridate)
loadSymbols('AAPL')
appleClosingPrice <- as.data.frame(Cl(AAPL))
head(appleClosingPrice)
appleClosingPrice$Day<- as.character(wday(rownames(appleClosingPrice), label=TRUE))
head(appleClosingPrice)  

library(ggplot2)
ggplot(appleClosingPrice, aes(x=Day)) + geom_histogram()








