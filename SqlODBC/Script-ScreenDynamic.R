# namespace
aNamespace <- "SqlODBC";
# path
if (Sys.getenv("AXONID") != "") {
  axonIDPath <- "";
  if (Sys.getenv("DataDrive") != "") {
    axonIDPath <- Sys.getenv("DataDrive");
  } else {
    axonIDPath <- Sys.getenv("SystemDrive");
  }
  axonIDPath <- paste0(c(axonIDPath, Sys.getenv("AXONID"), "R", aNamespace), sep = "/", collapse = "");
  setwd(axonIDPath);
}
getwd();
# base lib
gDriveUtil <- paste0(c("Lib/", "base", ".Util.R"), sep = "", collapse = "");
write(paste0(c("Load Util........\t", gDriveUtil), sep = "", collapse = ""), stdout());
source(gDriveUtil);
# RODBC lib
rodbcUtil <- paste0(c("Lib/", "RODBC", ".Util.R"), sep = "", collapse = "");
write(paste0(c("Load Util........\t", rodbcUtil), sep = "", collapse = ""), stdout());
source(rodbcUtil);
# ggplot2 lib
ggplot2Util <- paste0(c("Lib/", "ggplot2", ".Util.R"), sep = "", collapse = "");
write(paste0(c("Load Util........\t", ggplot2Util), sep = "", collapse = ""), stdout());
source(ggplot2Util);
# wordcloud lib
wordcloudUtil <- paste0(c("Lib/", "wordcloud", ".Util.R"), sep = "", collapse = "");
write(paste0(c("Load Util........\t", wordcloudUtil), sep = "", collapse = ""), stdout());
source(wordcloudUtil);
# password
dbUser <- "sa";
dbPass <- GetDBPassword("AXONID_ENV");
# DB list
dbList <- c("mmq", "Claims_MMQ");
dbList <- sort(dbList);
#
for (dbInstance in dbList) {
  write(dbInstance, stdout());
  # RODBC SimmqDB
  library(RODBC);
  simmqODBC <- odbcConnect("SimmqDB", uid = dbUser, pwd = dbPass);
  # select DB
  sqlInstance <- paste0(c("USE ", dbInstance, ";"), sep = "", collapse = "");
  write(sqlInstance, stdout());
  sqlQuery(simmqODBC, sqlInstance);
  ## DB table footprint
  sqlFile <- "SQL/DB-Table_listFootprint.sql";
  tableFootprintDataFrame <- SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile));
  DataFrameToCsv(tableFootprintDataFrame,
                 SqlFileToCsv(sqlFile, dbInstance, "-TableFootprint"));
  # DB table footprint rm IndexName
  colnames(tableFootprintDataFrame);
  tableFootprintDataFrame$IndexName <- NULL;
  colnames(tableFootprintDataFrame);
  # DB table footprint total
  countTables <- nrow(tableFootprintDataFrame);
  # DB table footprint means
  tableWords <- TableFootprintAboveMeans(tableFootprintDataFrame);
  # DB table footprint wordcloud
  corpusWords <- Corpus(VectorSource(tableWords));
  inspect(corpusWords);
  termDocMatrixSortDesc <- sort(rowSums(as.matrix(TermDocumentMatrix(corpusWords))),
                                decreasing=TRUE);
  termDocDataFrameSortDesc <- data.frame(word = names(termDocMatrixSortDesc),
                                         freq = termDocMatrixSortDesc);
  DataFrameToCsv(termDocDataFrameSortDesc,
                 SqlFileToCsv(sqlFile, dbInstance, "-TableFootprintSortDesc"));
  WordcloudToPng(termDocDataFrameSortDesc,
                SqlFileToPng(sqlFile, dbInstance, "-TableFootprintSortDesc"));
  ## DB table IO
  sqlFile <- "SQL/DB-Table_listIO.sql";
  tableIODataFrame <- SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile));
  DataFrameToCsv(tableIODataFrame,
                 SqlFileToCsv(sqlFile, dbInstance, "-TableIO"));
  # DB table IO ratio stacked histogram
  ioRatioTableDF <- MinimizeTableIO(tableIODataFrame);
  DataFrameToCsv(ioRatioTableDF,
                 SqlFileToCsv(sqlFile, dbInstance, "-TableIORatios"));
  # DB table IO stacked plot
  DBTableRatioToStackeplot(ioRatioTableDF,
                           SqlFileToPng(sqlFile, dbInstance, "-TableIORatios"));
  # DB table IO Q plot
  DBTableRatioToQplot(tableIODataFrame,
                      SqlFileToPng(sqlFile, dbInstance, "-TableIORatiosQplot"));
  ## DB StoredProc IO
  sqlFile <- "SQL/DB-Procedure_listIO.sql";
  storedProcIODataFrame <- SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile));
  DataFrameToCsv(storedProcIODataFrame,
                 SqlFileToCsv(sqlFile, dbInstance, "-StoredProcIO"));
  
  if(nrow(storedProcIODataFrame)>1){
    # DB StoredProc IO ratio stacked histogram
    ioRatioTableDF <- MinimizeStoredProcIO(storedProcIODataFrame);
    DataFrameToCsv(ioRatioTableDF,
                   SqlFileToCsv(sqlFile, dbInstance, "-StoredProcIORatios"));
    # DB StoredProc IO stacked plot
    DBStoredProcRatioToStackeplot(ioRatioTableDF,
                                  SqlFileToPng(sqlFile, dbInstance, "-StoredProcIORatios"));
    # DB StoredProc IO Q plot
    DBStoredProcRatioToQplot(storedProcIODataFrame,
                             SqlFileToPng(sqlFile, dbInstance, "-StoredProcIORatiosQplot"));
  }
  # RODBC close
  odbcClose(simmqODBC);
}
