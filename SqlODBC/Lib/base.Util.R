#
#
#
GetDBPassword <- function(environmentVariable = "AXONID-ENV") {
  dbPass <- "";
  if (Sys.getenv(environmentVariable) == "") {
    dbPass <- "";
  } else {
    if (Sys.getenv(environmentVariable) == "PROD") {
      dbPass <- "!_simmmq01_!";
    } else {
      dbPass <- "!_simmq01_!";
    }
  }
  return(dbPass);
}
#
#
#
SetAxonIDPath <- function(aNamespace="") {
  axonIDPath <- "";
  if (Sys.getenv("DataDrive") != "") {
    axonIDPath <- Sys.getenv("DataDrive");
  } else {
    axonIDPath <- Sys.getenv("SystemDrive");
  }
  axonIDPath <- paste0(c(axonIDPath, Sys.getenv("AXONID"), "R", aNamespace), sep = "/", collapse = "");
  setwd(axonIDPath);
}
#
#
#
GetSqlFromFile <- function(aSqlFilePath = "") {
  sqlCommand <- "";
  sqlCommand <- readLines(aSqlFilePath);
  sqlCommand <- paste(sqlCommand, collapse = " ");
  write(sqlCommand, stdout());
  return(sqlCommand);
}
#
#
#
DataFrameToCsv <- function(dataFrame=NULL, csvFile=""){
  write.csv(dataFrame, csvFile, row.names=FALSE);
}
#
#
#
SqlFileToCsv <- function(sqlFilePath="", dbName="", chartName=""){
  # extension time
  timeNow <- format(Sys.time(), "-%Y-%m-%d_%Hh%M");
  timeExt <- paste0(timeNow, chartName, ".csv");
  # csv
  csvFilePath <- gsub(".sql", timeExt, sqlFilePath);
  csvFilePath <- gsub("SQL/", paste0("../../ODBC/", dbName, "-", Sys.getenv("AXONID_ENV"), "-"), csvFilePath);
  return(csvFilePath);
}
#
#
#
SqlFileToPng <- function(sqlFilePath="", dbName="", chartName=""){
  # extension time
  timeNow <- format(Sys.time(), "-%Y-%m-%d_%Hh%M");
  timeExt <- paste0(timeNow, chartName, ".png");
  # png
  filePath <- gsub(".sql", timeExt, sqlFilePath);
  filePath <- gsub("SQL/", paste0("../../ODBC/", dbName, "-", Sys.getenv("AXONID_ENV"), "-"), filePath);
  return(filePath);
}
#
#
#
SummarizeDBFunctionDataFrame <- function(objectDataFrame, functionFilter="Function"){
  colnames <- colnames(objectDataFrame);
  # separate functions
  functionsDataFrame <- subset(objectDataFrame, grepl(paste0("^", functionFilter), objectDataFrame[[1]]), drop = TRUE);
  # sum functions
  functionsDataFrame <- data.frame(functionFilter, sum(functionsDataFrame[[2]]));
  colnames(functionsDataFrame) <- colnames;
  # remove functions
  objectDataFrame <- subset(objectDataFrame, !grepl(paste0("^", functionFilter), objectDataFrame[[1]]), drop = TRUE);
  # add sum functions
  objectDataFrame <- rbind(objectDataFrame, functionsDataFrame);
  #
  return(objectDataFrame);
}
#
#
#
SummarizeAllDBFunctionDataFrame <- function(objectDataFrame, functionFilter="Function"){
  colnames <- colnames(objectDataFrame);
  # separate functions
  functionsDataFrame <- subset(objectDataFrame, grepl(paste0("^", functionFilter), objectDataFrame[[1]]), drop = TRUE);
  colnames <- c("FunctionTypes", "FunctionCount");
  colnames(functionsDataFrame) <- colnames;
  #
  return(functionsDataFrame);
}
#
#
#
DataFrameFromColumns <- function(aDataFrame = NULL,
                                      colName1 = "",
                                      colName2 = "",
                                      colName3 = ""){
  columnList <- c(colName1, colName2, colName3);
  write(columnList, stdout());
  indexList <- which(colnames(aDataFrame) %in% columnList);
  write(indexList, stdout());
  return(aDataFrame[indexList]);
}
#
#
#
DataFrameWithoutWithTotal <- function(intWithout = 1,
                                      intWith = 1,
                                      intTotal = 2,
                                      OutputType = ""){
  aDataFrame <- data.frame(NULL);
  aDataFrame[1, 1] <- intWithout;
  aDataFrame[1, 2] <- intWith;
  aDataFrame[1, 3] <- intTotal;
  colnames(aDataFrame) <- c(paste0(OutputType,"WithoutParameters"),
                            paste0(OutputType,"WithParameters"),
                            paste0(OutputType,"Total"));
  return(aDataFrame);
}
#
#
#
MinimizeStoredProcIO <- function(aDataFrame = NULL){
  aDataFrame$CachedTime <- NULL;
  aDataFrame$TotalElapsedTime <- NULL;
  aDataFrame$ExecutionCount <- NULL;
  aDataFrame$TotalPhysicalReads <- NULL;
  aDataFrame$LogicalWritesRatio <- aDataFrame$TotalLogicalWrites/(aDataFrame$TotalLogicalReads + aDataFrame$TotalLogicalWrites);
  aDataFrame$LogicalReadsRatio <- aDataFrame$TotalLogicalReads/(aDataFrame$TotalLogicalReads + aDataFrame$TotalLogicalWrites);
  aDataFrame$TotalLogicalWrites <- NULL;
  aDataFrame$TotalLogicalReads <- NULL;
  aDataFrame <- melt(aDataFrame, if.var=StoreProcName);
  return(aDataFrame);
}
#
#
#
MinimizeTableIO <- function(aDataFrame = NULL){
  ioRatioTableDF <- aDataFrame;
  ioRatioTableDF$ObjectSchema <- NULL;
  ioRatioTableDF$TotalReads <- NULL;
  ioRatioTableDF$TotalWrites <- NULL;
  ioRatioTableDF <- melt(ioRatioTableDF, if.var=ObjectName);
  return(ioRatioTableDF);
}
#
#
#
TableFootprintAboveMeans <- function(tableFootprintDataFrame = NULL){
  tableWords <- NULL;
  meanRecordCount <- mean(tableFootprintDataFrame$RecordCount);
  tableWords <- c(tableWords,
                  as.vector(tableFootprintDataFrame[which(tableFootprintDataFrame$RecordCount >= meanRecordCount),1]));
  meanTotalPages <- mean(tableFootprintDataFrame$TotalPages);
  tableWords <- c(tableWords,
                  as.vector(tableFootprintDataFrame[which(tableFootprintDataFrame$TotalPages >= meanTotalPages),1]));
  meanUsedPages <- mean(tableFootprintDataFrame$UsedPages);
  tableWords <- c(tableWords,
                  as.vector(tableFootprintDataFrame[which(tableFootprintDataFrame$UsedPages >= meanUsedPages),1]));
  meanDataPages <- mean(tableFootprintDataFrame$DataPages);
  tableWords <- c(tableWords,
                  as.vector(tableFootprintDataFrame[which(tableFootprintDataFrame$DataPages >= meanDataPages),1]));
  meanTotalSpaceMB <- mean(tableFootprintDataFrame$TotalSpaceMB);
  tableWords <- c(tableWords,
                  as.vector(tableFootprintDataFrame[which(tableFootprintDataFrame$TotalSpaceMB >= meanTotalSpaceMB),1]));
  meanUsedSpaceMB <- mean(tableFootprintDataFrame$UsedSpaceMB);
  tableWords <- c(tableWords,
                  as.vector(tableFootprintDataFrame[which(tableFootprintDataFrame$UsedSpaceMB >= meanUsedSpaceMB),1]));
  meanDataSpaceMB <- mean(tableFootprintDataFrame$DataSpaceMB);
  tableWords <- c(tableWords,
                  as.vector(tableFootprintDataFrame[which(tableFootprintDataFrame$DataSpaceMB >= meanDataSpaceMB),1]));
  return(tableWords);
}
#
#
#