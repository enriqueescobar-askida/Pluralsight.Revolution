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
# plotrix lib
plotrixUtil <- paste0(c("Lib/", "plotrix", ".Util.R"), sep = "", collapse = "");
write(paste0(c("Load Util........\t", plotrixUtil), sep = "", collapse = ""), stdout());
source(plotrixUtil);
# password
dbUser <- "sa";
dbPass <- GetDBPassword("AXONID_ENV");
# DB list
dbList <- c("mmq", "Claims_MMQ");
dbList <- sort(dbList);
# RODBC Open SimmqDB
library(RODBC);
simmqODBC <- odbcConnect("SimmqDB", uid = dbUser, pwd = dbPass);
# Server info
serverInstance <- SqlCountResultToString(simmqODBC, "Select @@SERVERNAME     AS SQLServerInstance;");
serverVersion <- SqlCountResultToString(simmqODBC, "Select @@VERSION        AS SQLServerVersion;");
serverService <- SqlCountResultToString(simmqODBC, "Select @@ServiceName    AS ServiceInstance;");
# Server Running
sqlFile <- "SQL/DB-ServerRunning_list.sql";
DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
               SqlFileToCsv(sqlFile, serverInstance));
# Server Linked
sqlFile <- "SQL/DB-ServerLinked_list.sql";
DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
               SqlFileToCsv(sqlFile, serverInstance));
# Server DB spec
sqlFile <- "SQL/DB-ServerDBSpec_list.sql";
DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
               SqlFileToCsv(sqlFile, serverInstance));
# Server DB Backup
sqlFile <- "SQL/DB-ServerDBBackup_list.sql";
DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
               SqlFileToCsv(sqlFile, serverInstance));
## Server DB Usage
# Server DB usage list
sqlFile <- "SQL/DB-Usage_list.sql";
sqlDataFrame <- SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile));
DataFrameToCsv(sqlDataFrame,
               SqlFileToCsv(sqlFile, serverInstance));
# Server DB usage plots
DBUsageDataFrameToBarplot(sqlDataFrame,
                          SqlFileToPng(sqlFile, serverInstance, "-Barplot"));
DBUsageDataFrameToPiechart(sqlDataFrame,
                           SqlFileToPng(sqlFile, serverInstance, "-Piechart"));
###
for (dbInstance in dbList) {
  write(dbInstance, stdout());
  # select DB
  sqlInstance <- paste0(c("USE ", dbInstance, ";"), sep = "", collapse = "");
  write(sqlInstance, stdout());
  sqlQuery(simmqODBC, sqlInstance);
  ## DB All parameter
  # DB All parameter list
  sqlFile <- "SQL/DB-ParametersInFuncsAndProcs.sql";
  allParametersDataFrame <- SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile));
  DataFrameToCsv(allParametersDataFrame,
                 SqlFileToCsv(sqlFile, dbInstance));
  ## DB Object
  # DB object list
  sqlFile <- "SQL/DB-Object_list.sql";
  ObjectListDataFrame <- SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile));
  DataFrameToCsv(ObjectListDataFrame,
                 SqlFileToCsv(sqlFile, dbInstance));
  # DB object list sum fonctions
  ObjectSumDataFrame <- SummarizeDBFunctionDataFrame(ObjectListDataFrame, "Function");
  DBObjectDataFrameToBarplot(ObjectSumDataFrame,
                             SqlFileToPng(sqlFile, dbInstance, "-Sum_Barplot"));
  # DB object list all fonctions
  objectAllDataFrame <- SummarizeAllDBFunctionDataFrame(ObjectListDataFrame, "Function");
  DBFunctionDataFrameToBarplot(objectAllDataFrame,
                               SqlFileToPng(sqlFile, dbInstance, "-All_Barplot"));
  DBFunctionDataFrameToPiechart(objectAllDataFrame,
                                SqlFileToPng(sqlFile, dbInstance, "-All_Piechart"));
  ## DB StoredProc
  # DB StoreProc count
  sqlFile <- "SQL/DB-Procedure_count.sql";
  countStoreProc <- SqlCountResultToInteger(simmqODBC, GetSqlFromFile(sqlFile));
  # DB StoreProc list
  sqlFile <- "SQL/DB-Procedure_list.sql";
  DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
                 SqlFileToCsv(sqlFile, dbInstance));
  # DB StoreProc param list
  sqlFile <- "SQL/DB-Procedure_listParams.sql";
  storedProcParamDataFrame <- SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile));
  DataFrameToCsv(storedProcParamDataFrame,
                 SqlFileToCsv(sqlFile, dbInstance));
  storedProcParamDataFrameSlim <- DataFrameFromColumns(storedProcParamDataFrame,
                                                        "ProcedureName",
                                                        "ProcedureType",
                                                        "ProcedureDesc");
  # DB StoreProc repeat count
  storedProcParamDataFrameFat <- aggregate(list(NbParameters = rep(1, nrow(storedProcParamDataFrameSlim))),
                                            storedProcParamDataFrameSlim,
                                            length);
  write(summary(rev(storedProcParamDataFrameFat)[1]), stdout());
  # DB StoreProc with params
  countStoreProcWith <- nrow(storedProcParamDataFrameFat);
  # DB StoreProc without params
  countStoreProcWithout <- countStoreProc - countStoreProcWith;
  # DB StoreProc data frame params
  storeProcParamsDF <- DataFrameWithoutWithTotal(countStoreProcWithout,
                                                 countStoreProcWith,
                                                 countStoreProc,
                                                 "StoreProc");
  # DB StoreProc param list export
  DataFrameToCsv(storeProcParamsDF,
                 SqlFileToCsv(sqlFile, dbInstance, "-Procs_Specs"));
  # DB StoreProc barplot
  StoredProcWithoutWithTotalDFToBarplot(storeProcParamsDF,
                                        SqlFileToPng(sqlFile, dbInstance, "-Procs_Barplot"),
                                        "StoredProc");
  # DB StoreProc boxplot
  DBStoreProcDataFrameToBoxplot(storedProcParamDataFrameFat,
                                SqlFileToPng(sqlFile, dbInstance, "-Procs_Boxplot"));
  # DB StoreProc density plot
  DBStoreProcDataFrameToDensityplot(storedProcParamDataFrameFat,
                                    SqlFileToPng(sqlFile, dbInstance, "-Procs_Densityplot"));
  ## DB Table Count
  # DB Table Count DROP
  sqlQuery(simmqODBC, "IF OBJECT_ID(N'#counts', N'U') IS NOT NULL DROP TABLE #counts;")
  # DB TABLE Count CREATE
  sqlQuery(simmqODBC, "CREATE TABLE #counts (TableName VARCHAR(255), TableRows INT);");
  # DB Table Count EXEC
  sqlQuery(simmqODBC, "EXEC sp_MSForEachTable @command1='INSERT #counts (TableName, TableRows) SELECT ''?'', COUNT(*) FROM ?';");
  # DB Table Count CSV
  sqlFile <- "SQL/DB-RowCount_list.sql";
  tableRowCountDataFrame <- SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile));
  DataFrameToCsv(tableRowCountDataFrame,
                 SqlFileToCsv(sqlFile, dbInstance));
  # DB Table Count repeats
  tableRowCountDataFrame <- aggregate(list(RowRepeats = rep(1, nrow(tableRowCountDataFrame[-1]))),
                                   tableRowCountDataFrame[-1],
                                   length);
  # DB Table Count half > mean repeats
  aMean <- mean(tableRowCountDataFrame$RowRepeats);
  tableRowCountDataFrame <- subset(tableRowCountDataFrame, RowRepeats > aMean);
  DBRowCountFrameToBarplot(tableRowCountDataFrame,
                           SqlFileToPng(sqlFile, dbInstance, "_Barplot"));
  # DB Table Count DROP
  sqlQuery(simmqODBC, "DROP TABLE #counts;");
  ## DB Table Analysis
  # DB Table Analysis count
  sqlFile <- "SQL/DB-Table_count.sql";
  countTable <- SqlCountResultToInteger(simmqODBC, GetSqlFromFile(sqlFile));
  # DB Table Analysis list
  sqlFile <- "SQL/DB-Table_list.sql";
  DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
                 SqlFileToCsv(sqlFile, dbInstance));
  # DB Table Analysis Independent
  sqlFile <- "SQL/DB-TableIndependent_list.sql";
  DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
                 SqlFileToCsv(sqlFile, dbInstance));
  # DB Table Analysis Trunk
  sqlFile <- "SQL/DB-TableTrunk_list.sql";
  DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
                 SqlFileToCsv(sqlFile, dbInstance));
  # DB Table Analysis Branch
  sqlFile <- "SQL/DB-TableBranch_list.sql";
  DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
                 SqlFileToCsv(sqlFile, dbInstance));
  # DB Table Analysis Leaf
  sqlFile <- "SQL/DB-TableLeaf_list.sql";
  DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
                 SqlFileToCsv(sqlFile, dbInstance));
  # DB Table Analysis Parent
  sqlFile <- "SQL/DB-TableParent_list.sql";
  DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
                 SqlFileToCsv(sqlFile, dbInstance));
  # DB Table Analysis Child
  sqlFile <- "SQL/DB-TableChild_list.sql";
  DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
                 SqlFileToCsv(sqlFile, dbInstance));
  # DB Table Analysis SelfRef
  sqlFile <- "SQL/DB-TableSelfRef_list.sql";
  DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
                 SqlFileToCsv(sqlFile, dbInstance));
  # DB Table Analysis keys list
  sqlFile <- "SQL/DB-Table_listKeys.sql";
  DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
                 SqlFileToCsv(sqlFile, dbInstance));
  ## View Analysis
  # View Analysis count
  sqlFile <- "SQL/DB-View_count.sql";
  countView <- SqlCountResultToInteger(simmqODBC, GetSqlFromFile(sqlFile));
  # View Analysis list
  sqlFile <- "SQL/DB-View_list.sql";
  DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
                 SqlFileToCsv(sqlFile, dbInstance));
  ## Function Analysis
  # Function Analysis count
  sqlFile <- "SQL/DB-Function_count.sql";
  countFunc <- SqlCountResultToInteger(simmqODBC, GetSqlFromFile(sqlFile));
  # Function Analysis list
  sqlFile <- "SQL/DB-Function_list.sql";
  DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
                 SqlFileToCsv(sqlFile, dbInstance));
  # Function Analysis param list
  sqlFile <- "SQL/DB-Function_listParams.sql";
  functionParamsDataFrame <- SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile));
  DataFrameToCsv(functionParamsDataFrame,
                 SqlFileToCsv(sqlFile, dbInstance));
  fnParamsDataFrameSlim <- DataFrameFromColumns(functionParamsDataFrame,
                                                "FunctionName",
                                                "FunctionType",
                                                "FunctionDesc");
  # Function Analysis repeat count
  fnParamsDataFrameFat <- aggregate(list(NbParameters = rep(1, nrow(fnParamsDataFrameSlim))),
                                    fnParamsDataFrameSlim,
                                    length);
  write(summary(rev(fnParamsDataFrameFat)[1]), stdout());
  # Function Analysis without params
  countFuncWith <- nrow(fnParamsDataFrameFat);
  # Function Analysis without params
  countFuncWithout <- countFunc - countFuncWith;
  # Function Analysis data frame params
  fnParamsDF <- DataFrameWithoutWithTotal(countFuncWithout,
                                          countFuncWith,
                                          countFunc,
                                          "Fn");
  # Function Analysis param list export
  DataFrameToCsv(fnParamsDF,
                 SqlFileToCsv(sqlFile, dbInstance, "-Funcs_Specs"));
  # Function Analysis barplot
  FunctionWithoutWithTotalDFToBarplot(fnParamsDF,
                                      SqlFileToPng(sqlFile, dbInstance, "-Funcs_Barplot"),
                                      "Fn");
  # Function Analysis boxplot
  DBFunctionDataFrameToBoxplot(fnParamsDataFrameFat,
                               SqlFileToPng(sqlFile, dbInstance, "-Funcs_Boxplot"));
  # Function Analysis density plot
  DBFunctionDataFrameToDensityplot(fnParamsDataFrameFat,
                                   SqlFileToPng(sqlFile, dbInstance, "-Funcs_Densityplot"));
  ## PKey Analysis
  # PKey Analysis count
  sqlFile <- "SQL/DB-PKeys_count.sql";
  countPKeys <- SqlCountResultToInteger(simmqODBC, GetSqlFromFile(sqlFile));
  # PKey Analysis list
  sqlFile <- "SQL/DB-PKeys_list.sql";
  DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
                 SqlFileToCsv(sqlFile, dbInstance));
  ## FKey Analysis
  # FKey Analysis count
  sqlFile <- "SQL/DB-FKeys_count.sql";
  countFKeys <- SqlCountResultToInteger(simmqODBC, GetSqlFromFile(sqlFile));
  # FKey Analysis list
  sqlFile <- "SQL/DB-FKeys_list.sql";
  DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
                 SqlFileToCsv(sqlFile, dbInstance));
  ## Index Analysis
  # Index Analysis count
  sqlFile <- "SQL/DB-Index_count.sql";
  countIndex <- SqlCountResultToInteger(simmqODBC, GetSqlFromFile(sqlFile));
  # Index Analysis list
  sqlFile <- "SQL/DB-Index_list.sql";
  DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
                 SqlFileToCsv(sqlFile, dbInstance));
  # Index Analysis type list
  sqlFile <- "SQL/DB-Index_listTypes.sql";
  DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
                 SqlFileToCsv(sqlFile, dbInstance));
  ## Constraint Analysis
  # Constraint Analysis List
  sqlFile <- "SQL/DB-Constraint_list.sql";
  DataFrameToCsv(SqlResultToDataFrame(simmqODBC, GetSqlFromFile(sqlFile)),
                 SqlFileToCsv(sqlFile, dbInstance));
}
# RODBC close SimmqDB
odbcClose(simmqODBC);
