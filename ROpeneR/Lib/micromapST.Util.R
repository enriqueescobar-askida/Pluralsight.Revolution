micromapSTLib <- paste0(c("Lib/", "import_micromapST.R"), sep = "", collapse = "");
write(paste0(c("Load Lib ........\t", micromapSTLib), sep = "", collapse = ""), stdout());
source(micromapSTLib);
#
#
#
MicromapPlot <- function(dataDF = data.frame(NULL),
                        panelDF = data.frame(NULL),
                        rowNamesDF = "full",
                        sortVariable = "Age",
                        ascendent = FALSE,
                        mainTitle = "",
                        subTitle = ""){
  
  micromapST(acs2014,
             panelDesc,
             rowNames = rowNamesDF,
             sortVar= sortVariable,
             ascend = ascendent,
             title = paste(mainTitle, subTitle),
             details = list(Title.cex = 1.4));
  
}
