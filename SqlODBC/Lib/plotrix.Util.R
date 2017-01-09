#
#
#
library(plotrix);
DBFunctionDataFrameToPiechart <- function(functionDataFrame = NULL,
                                          pngFilePath = "") {
  if(is.null(functionDataFrame)){
    file.create(pngFilePath);
  }else{
    write(colnames(functionDataFrame), stdout());
    PercentList <- round(functionDataFrame$FunctionCount / sum(functionDataFrame$FunctionCount) * 100);
    ColorList <- c("#66FF99", "#6699CC", "#FF5050");
    write(PercentList, stdout());
    # titles
    xTitle <- colnames(functionDataFrame)[1];
    yTitle <- colnames(functionDataFrame)[-1];
    mainTitle <- "Function Type List Piechart";
    # graph
    png(filename = pngFilePath, width = 800, height = 800);
    pie3D(functionDataFrame$FunctionCount,
          labels = paste(functionDataFrame$FunctionTypes, paste("%", PercentList)),
          main = mainTitle,
          theta = pi / 3,
          col = ColorList);
    dev.off();
  }
}
#
#
#