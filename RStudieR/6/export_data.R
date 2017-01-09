library(ggplot2)
df<-diamonds
setwd('~/R_exports')
getwd()
write.table(df, 
            file = "diamonds.csv", 
            sep = ",", 
            col.names = colnames(diamonds),
            qmethod = "escape")

write.csv(df, "diamonds2.csv")

system('opendiff diamonds.csv diamonds2.csv')




library(foreign)
write.foreign(df, "diamonds.txt", "diamonds.sps", package="SPSS")













save(df, file='diamonds.RData')
rm(df)
load('diamonds.RData')










