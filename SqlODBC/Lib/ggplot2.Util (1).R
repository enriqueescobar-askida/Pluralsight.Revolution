library(ggplot2);
#
#
#
DBUsageDataFrameToPiechart <- function(usageDataFrame = NULL,
                                       pngFilePath = "") {
  if(is.null(usageDataFrame)){
    file.create(pngFilePath);
  }else{
    write(colnames(usageDataFrame), stdout());
    PercentList <- round(usageDataFrame$DBBufferMB / sum(usageDataFrame$DBBufferMB) * 100, digits = 1);
    # ColorList <- c("#66FF99", "#6699CC", "#FF5050");
    ColorList <- heat.colors(length(PercentList));
    write(PercentList, stdout());
    # titles
    xTitle <- colnames(usageDataFrame)[2];
    yTitle <- colnames(rev(usageDataFrame)[1]);
    mainTitle <- "Usage List Piechart";
    # graph
    png(filename = pngFilePath, width = 800, height = 800);
    pie3D(usageDataFrame$DBBufferMB,
          labels = paste(usageDataFrame$DBName, paste0(PercentList, "%")),
          main = mainTitle,
          theta = pi / 3,
          col = ColorList);
    dev.off();
  }
}
#
#
#
DBUsageDataFrameToBarplot <- function(usageDataFrame = NULL,
                                      pngFilePath = "") {
  if(is.null(usageDataFrame)){
    file.create(pngFilePath);
  }else{
    write(colnames(usageDataFrame), stdout());
    # titles
    xTitle <- colnames(usageDataFrame)[2];
    yTitle <- colnames(rev(usageDataFrame)[1]);
    mainTitle <- "Usage List count";
    write(xTitle, stdout());
    write(yTitle, stdout());
    write(mainTitle, stdout());
    # graph
    barplot <- ggplot(usageDataFrame, aes(x = factor(DBName), y = DBBufferMB)) +
      geom_bar(stat = "identity", width = 0.8, position = "dodge", fill = "lightblue") +
      xlab(xTitle) + ylab(yTitle) + ggtitle(mainTitle) +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5));
    barplot;
    ggsave(filename = pngFilePath, plot = barplot, dpi = 100);
  }
}
#
#
#
DBRowCountFrameToBarplot <- function(usageDataFrame = NULL,
                                      pngFilePath = "") {
  if(is.null(usageDataFrame)){
    file.create(pngFilePath);
  }else{
    write(colnames(usageDataFrame), stdout());
    # titles
    xTitle <- colnames(usageDataFrame)[1];
    yTitle <- colnames(rev(usageDataFrame)[1]);
    mainTitle <- "Row List count";
    # graph
    barplot <- ggplot(usageDataFrame, aes(x = factor(TableRows), y = RowRepeats)) +
    ##barplot <- ggplot(usageDataFrame, aes(x = factor(TableRows), y = sqrt(RowRepeats))) +
      geom_bar(stat = "identity", width = 0.8, position = "dodge", fill = "lightblue") +
      ##scale_y_sqrt(paste0("Square root of ", yTitle)) +
      xlab(xTitle) + ylab(yTitle) + ggtitle(mainTitle) +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5));
    barplot;
    ggsave(filename = pngFilePath, plot = barplot, dpi = 100);
  }
}
#
#
#
DBObjectDataFrameToBarplot <- function(objectDataFrame = NULL,
                                       pngFilePath = "") {
  if(is.null(objectDataFrame)){
    file.create(pngFilePath);
  }else{
    write(colnames(objectDataFrame), stdout());
    # titles
    xTitle <- colnames(objectDataFrame)[1];
    yTitle <- colnames(objectDataFrame)[-1];
    mainTitle <- "ObjectList count";
    # graph
    barplot <- ggplot(objectDataFrame, aes(x = factor(ObjectName), y = ObjectCount)) +
      geom_bar(stat = "identity", width = 0.8, position = "dodge", fill = "lightblue") +
      xlab(xTitle) + ylab(yTitle) + ggtitle(mainTitle);
    barplot;
    ggsave(filename = pngFilePath, plot = barplot, dpi = 100);
  }
}
#
#
#
DBFunctionDataFrameToBarplot <- function(functionDataFrame = NULL,
                                         pngFilePath = "") {
  if(is.null(functionDataFrame)){
    file.create(pngFilePath);
  }else{
    write(colnames(functionDataFrame), stdout());
    # titles
    xTitle <- colnames(functionDataFrame)[1];
    yTitle <- colnames(functionDataFrame)[-1];
    mainTitle <- "Function Types Barplot";
    # graph
    aBarplot <- ggplot(functionDataFrame,
                       aes(x = "", y = FunctionCount, fill = FunctionTypes)) +
      labs(fill = xTitle) + geom_bar(width = 1, stat = "identity") +
      xlab(xTitle) + ylab(yTitle) + ggtitle(mainTitle);
    aBarplot;
    ggsave(filename = pngFilePath, plot = aBarplot, dpi = 100);
  }
}
#
#
#
DBFunctionDataFrameToBoxplot <- function(functionDataFrame = NULL,
                                         pngFilePath = "") {
  if(is.null(functionDataFrame)){
    file.create(pngFilePath);
  }else{
    write(colnames(functionDataFrame), stdout());
    # titles
    xTitle <- "Function type";
    yTitle <- "Number of parameters";
    mainTitle <- "Boxplot for parameter distribution";
    # graph
    aBoxplot <-
      ggplot(functionDataFrame, aes(x = FunctionType, y = NbParameters)) + geom_boxplot(aes(fill = FunctionType)) +
      geom_jitter() +
      # + geom_point(aes(colour = factor(type_desc)), size=4)
      scale_y_continuous(breaks = seq(0, 12, 1.0)) + labs(title = mainTitle, x = xTitle, y = yTitle);
    aBoxplot;
    ggsave(filename = pngFilePath, plot = aBoxplot, dpi = 100);
  }
}
#
#
#
DBFunctionDataFrameToDensityplot <- function(functionDataFrame = NULL,
                                             pngFilePath = "") {
  if(is.null(functionDataFrame)){
    file.create(pngFilePath);
  }else{
    write(colnames(functionDataFrame), stdout());
    # titles
    xTitle <- "Number of parameters";
    mainTitle <- "Densityplot for parameter distribution";
    # graph
    aDensityplot <-
      ggplot(functionDataFrame, aes(NbParameters, fill = FunctionType)) +
      geom_density(alpha = 0.6) + labs(title = mainTitle, x = xTitle);
    aDensityplot;
    ggsave(filename = pngFilePath, plot = aDensityplot, dpi = 100);
  }
}
#
#
#
StoredProcWithoutWithTotalDFToBarplot <- function(woWithTotalDataFrame = NULL,
                                               pngFilePath = "",
                                               OutputType = "") {
  if(is.null(woWithTotalDataFrame)){
    file.create(pngFilePath);
  }else{
    # rm totals/ last column & save colnames
    woWithTotalDataFrame <- rev(woWithTotalDataFrame)[-1];
    myColNames <- colnames(woWithTotalDataFrame);
    # rm colname and transpose and rm new colname
    colnames(woWithTotalDataFrame) <- NULL;
    woWithTotalDataFrame <- t(woWithTotalDataFrame);
    colnames(woWithTotalDataFrame) <- NULL;
    # add columns and replace colname 
    woWithTotalDataFrame <- cbind(myColNames, woWithTotalDataFrame);
    colnames(woWithTotalDataFrame) <- NULL;
    colnames(woWithTotalDataFrame) <- c(paste0(OutputType, "Types"), paste0(OutputType, "Count"));
    # cast to data frame
    woWithTotalDataFrame <- as.data.frame(woWithTotalDataFrame);
    # check colname
    write(colnames(woWithTotalDataFrame), stdout());
    # titles
    xTitle <- colnames(woWithTotalDataFrame)[1];
    yTitle <- colnames(woWithTotalDataFrame)[-1];
    mainTitle <- paste0(OutputType, " Type List Barplot");
    # graph
    aBarplot <- ggplot(woWithTotalDataFrame,
                       aes(x = "", y = StoredProcCount, fill = StoredProcTypes)) +
      labs(fill = xTitle) + geom_bar(width = 1, stat = "identity") +
      xlab(xTitle) + ylab(yTitle) + ggtitle(mainTitle);
    aBarplot;
    ggsave(filename = pngFilePath, plot = aBarplot, dpi = 100);
  }
}
#
#
#
FunctionWithoutWithTotalDFToBarplot <- function(woWithTotalDataFrame = NULL,
                                                  pngFilePath = "",
                                                  OutputType = "") {
  if(is.null(woWithTotalDataFrame)){
    file.create(pngFilePath);
  }else{
    # rm totals/ last column & save colnames
    woWithTotalDataFrame <- rev(woWithTotalDataFrame)[-1];
    myColNames <- colnames(woWithTotalDataFrame);
    # rm colname and transpose and rm new colname
    colnames(woWithTotalDataFrame) <- NULL;
    woWithTotalDataFrame <- t(woWithTotalDataFrame);
    colnames(woWithTotalDataFrame) <- NULL;
    # add columns and replace colname 
    woWithTotalDataFrame <- cbind(myColNames, woWithTotalDataFrame);
    colnames(woWithTotalDataFrame) <- NULL;
    colnames(woWithTotalDataFrame) <- c(paste0(OutputType, "Types"), paste0(OutputType, "Count"));
    # cast to data frame
    woWithTotalDataFrame <- as.data.frame(woWithTotalDataFrame);
    # check colname
    write(colnames(woWithTotalDataFrame), stdout());
    # titles
    xTitle <- colnames(woWithTotalDataFrame)[1];
    yTitle <- colnames(woWithTotalDataFrame)[-1];
    mainTitle <- paste0(OutputType, " Type List Barplot");
    # graph
    aBarplot <- ggplot(woWithTotalDataFrame,
                       aes(x = "", y = FnCount, fill = FnTypes)) +
      labs(fill = xTitle) + geom_bar(width = 1, stat = "identity") +
      xlab(xTitle) + ylab(yTitle) + ggtitle(mainTitle);
    aBarplot;
    ggsave(filename = pngFilePath, plot = aBarplot, dpi = 100);
  }
}
#
#
#
DBStoreProcDataFrameToBoxplot <- function(storeProcDataFrame = NULL,
                                          pngFilePath = "") {
  if(is.null(storeProcDataFrame)){
    file.create(pngFilePath);
  }else{
    write(colnames(storeProcDataFrame), stdout());
    # titles
    xTitle <- "Procedure type";
    yTitle <- "Number of parameters";
    mainTitle <- "Boxplot for parameter distribution";
    # graph
    aBoxplot <-
      ggplot(storeProcDataFrame, aes(x = ProcedureType, y = NbParameters)) + geom_boxplot(aes(fill = ProcedureType)) +
      geom_jitter() +
      # + geom_point(aes(colour = factor(type_desc)), size=4)
      scale_y_continuous(breaks = seq(0, 12, 1.0)) + labs(title = mainTitle, x = xTitle, y = yTitle);
    aBoxplot;
    ggsave(filename = pngFilePath, plot = aBoxplot, dpi = 100);
  }
}
#
#
#
DBStoreProcDataFrameToDensityplot <- function(storeProcDataFrame = NULL,
                                              pngFilePath = "") {
  if(is.null(storeProcDataFrame)){
    file.create(pngFilePath);
  }else{
    write(colnames(storeProcDataFrame), stdout());
    # titles
    xTitle <- "Number of parameters";
    mainTitle <- "Densityplot for parameter distribution";
    # graph
    aDensityplot <-
      ggplot(storeProcDataFrame, aes(NbParameters, fill = ProcedureType)) +
      geom_density(alpha = 0.6) + labs(title = mainTitle, x = xTitle);
    aDensityplot;
    ggsave(filename = pngFilePath, plot = aDensityplot, dpi = 100);
  }
}
#
#
#
library(reshape2);
DBTableRatioToStackeplot <- function(ioRatioTableDF = NULL,
                                     pngFilePath = "") {
  stackedPlot <- ggplot(ioRatioTableDF, aes(x=ObjectName, y=value, fill=variable)) +
    geom_bar(stat="identity") +
    ylab("IO Ratio (Max 1)") + xlab("TableName") + ggtitle("IO Ratio by TableName") +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5));
  stackedPlot;
  ggsave(filename = pngFilePath, plot = stackedPlot, dpi = 100);
}
#
#
#
DBTableRatioToQplot <- function(tableIODataFrame = NULL,
                                pngFilePath = "") {
  ioReadTableDF <- tableIODataFrame[,c(2,5,6)];
  ioReadTableDF <- melt(ioReadTableDF, if.var=ObjectName);
  # qPlot <- ggplot(data=ioReadTableDF, aes(x=ObjectName, y=sqrt(value), colours(variable))) +
  qPlot <- ggplot(data=ioReadTableDF, aes(x=ObjectName, y=value, colours(variable))) +
    geom_line() + geom_point(aes(color=factor(variable))) + stat_smooth() +
    # scale_y_sqrt(label_value("Square root Nb IO")) +
    xlab("TableName") + ggtitle("IO count by ObjectName") +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5));
  qPlot;
  ggsave(filename = pngFilePath, plot = qPlot, dpi = 100);
}
#
#
#
DBStoredProcRatioToStackeplot <- function(ioReadTableDF = NULL,
                                pngFilePath = "") {
  #qPlot <- ggplot(data=ioReadTableDF, aes(x=StoreProcName, y=sqrt(value), colours(variable))) +
  qPlot <- ggplot(data=ioReadTableDF, aes(x=StoreProcName, y=value, colours(variable))) +
    geom_line() + geom_point(aes(color=factor(variable))) + stat_smooth() +
    # scale_y_sqrt(label_value("Square root Nb IO")) +
    xlab("TableName") + ggtitle("IO count by TableName") +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5));
  qPlot;
  ggsave(filename = pngFilePath, plot = qPlot, dpi = 100);
}
#
#
#
DBStoredProcRatioToQplot <- function(storedProcIODataFrame = NULL,
                                pngFilePath = "") {
  ioReadTableDF <- storedProcIODataFrame[,c(1,6,7)];
  ioReadTableDF <- melt(ioReadTableDF, if.var=StoreProcName);
  # qPlot <- ggplot(data=ioReadTableDF, aes(x=StoreProcName, y=sqrt(value), colours(variable))) +
  qPlot <- ggplot(data=ioReadTableDF, aes(x=StoreProcName, y=value, colours(variable))) +
    geom_line() + geom_point(aes(color=factor(variable))) + stat_smooth() +
    # scale_y_sqrt(label_value("Square root Nb IO")) +
    xlab("TableName") + ggtitle("IO count by StoreProcName") +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5));
  qPlot;
  ggsave(filename = pngFilePath, plot = qPlot, dpi = 100);
}
#
#
#