library(RODBC);
#
#
#
SqlResultToDataFrame <- function(odbcConnector = NULL, sqlSelectCount = "") {
  dataFrame <- sqlQuery(odbcConnector, sqlSelectCount);
  nbRows <- nrow(dataFrame);
  write(nbRows, stdout());
  #if(nbRows < 1){
  #  return(NULL);
  #}else{
  return(dataFrame);
  #}
}
#
#
#
SqlCountResultToInteger <- function(odbcConnector = NULL, sqlSelectCount = "") {
  dataFrame <- sqlQuery(odbcConnector, sqlSelectCount);
  anInteger <- dataFrame[[1]];
  write(anInteger, stdout());
  if(is.integer(anInteger)){
    return(anInteger);
  }else{
    return(-1);
  }
}
#
#
#
SqlCountResultToString <- function(odbcConnector = NULL, sqlSelectString = "") {
  dataFrame <- sqlQuery(odbcConnector, sqlSelectString);
  aString <- as.character(dataFrame[[1]]);
  write(aString, stdout());
  if(is.character(aString)){
    return(aString);
  }else{
    return("");
  }
}