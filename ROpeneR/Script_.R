# env var
gDriveHome <- gsub("\\\\", "/", Sys.getenv("GDrive"));
#shared folder
gDriveFolder <- "MiApp";
#namespace
gDriveProject <- "Pluralsight.Revolution";
write(paste0(c("Namespace........\t", gDriveProject), sep = "", collapse = ""), stdout());
#environment
gDriveSpace <- "Ropen";
#path
write(paste0(c("Environment......\t", gDriveSpace), sep = "", collapse = ""), stdout());
gDriveList <- c(gDriveHome, gDriveFolder, gDriveProject, gDriveSpace);
gDrivePath <- paste0(gDriveList, sep = "/", collapse = "");
write(paste0(c("Path old.........\t", getwd()), sep = "", collapse = ""), stderr());
setwd(gDrivePath);
write(paste0(c("Path.............\t", getwd()), sep = "", collapse = ""), stdout());
#doc
gDriveDoc <- paste0(c(gDriveSpace, "Doc"), sep = "/", collapse = "");
write(paste0(c("Doc folder.......\t", gDriveDoc), sep = "", collapse = ""), stdout());
#lib
gDriveLib <- paste0(c(gDriveSpace, "Lib"), sep = "/", collapse = "");
write(paste0(c("Lib folder.......\t", gDriveLib), sep = "", collapse = ""), stdout());
#util
gDriveUtil <- paste0(c("Lib/", gDriveSpace, ".Util.R"), sep = "", collapse = "");
write(paste0(c("Load Util........\t", gDriveUtil), sep = "", collapse = ""), stdout());
source(gDriveUtil);
#
#
#
TFR <- c(2.6, 1.9, 2.0, 3.3, 2.5, 2.3, 2.5);
names(TFR) <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama");
VectorToHorizontalBarplot(TFR, "Central America, Fertility Rate 2012", "average births per woman");
#
#
#
countries2012 <- read.csv("Doc/countries2012.csv");
index <- seq(from = 1,to = 179, by = 4);
sample <- countries2012$TFR[index];
names(sample) <- countries2012$COUNTRY[index];
sample <- sample[order(sample)];

par(mar = c(5, 10, 4, 2));

NumericToChartSolid(sample,
			cex = 0.8,
			pch = 16,
			xlim = c(1,7),
			main = "Total Fertility Rate by Country",
			xlab = "average births per woman",
			adj = 1,
			cex.main = 2,
			cex.lab = 1.5);

abline (v=2, col = "red");
text (2, 12.5, "replacement rate", cex = 0.7, pos = 4, col = "red");
#
#
###
#"Fertility Rate Distributions by Continent"
#"average births per woman"
#"<- replacement rate"
countries2012$CONTINENT <- reorder(countries2012$CONTINENT, countries2012$TFR, median);
mini <- round(floor(min(countries2012$TF)), 0);
maxi <- round(ceiling(max(countries2012$TFR)), 0);
BoxplotWithFormula(TFR ~ CONTINENT, countries2012, TRUE, mini, maxi,
				   "Fertility Rate Distributions by Continent",
				   "average births per woman",
				   "<- replacement rate");
#
#
#
VectorToHistogram(countries2012$TFR, "Fertility Rate by Country", "average births per woman", FALSE);
VectorToHistogram(countries2012$TFR, "Fertility Rate by Country", "average births per woman", TRUE);
#
#"U.S. Population, 2005 - 2015"
#"millions of people"
List1 <- seq(2005, 2015);
List2 <- c(296, 298, 301, 304, 307, 308, 311, 313, 315, 318, 320);
aDataFrame <- data.frame(List1, List2);
DataFrame2ListToLinechart(aDataFrame, "U.S. Population, 2005 - 2015", "", "millions of people");
#
#"UK Road Casualties 1969-1984"
#"monthly driver deaths"
aTimeSerie <- UKDriverDeaths;
TimeSerieToLinets(aTimeSerie, "UK Road Casualties 1969-1984", "monthly driver deaths", "");
#
#"UK Road Casualties 1969-1984"
#"driver deaths"
TimeSerieToMonthplot(aTimeSerie, "UK Road Casualties 1969-1984", "", "driver deaths");
#"Total Fertility Rate vs. GDP, 2012"
#"GDP per capita (in 1000s $US)"
#"average births per woman"
CountriesDataFrameToScatterplot(countries2012, "Total Fertility Rate vs. GDP, 2012", "GDP per capita (in 1000s $US)", "average births per woman");
#"Total Fertility Rate vs. GDP, 2012"
#"GDP per capita (in 1000s $US)"
#"average births per woman"
CountriesDataFrameToColorScatterplot(countries2012, "Total Fertility Rate vs. GDP, 2012", "GDP per capita (in 1000s $US)", "average births per woman");
#"Total Fertility Rate vs. GDP, 2012"
#"GDP per capita (in 1000s $US)"
#"average births per woman"
CountryFromDataFrameToColorScatterplot(countries2012, "Total Fertility Rate vs. GDP, 2012", "GDP per capita (in 1000s $US)", "average births per woman", "United States");
#"World Development Indicators by Continent"
#
#
CountriesToParallelPlot(countries2012, "World Development Indicators by Continent");
#"World Development"
#"Indicator Correlations"
#
CountriesToSplom(countries2012, "World Development", "Indicator Correlations", TRUE);
CountriesToSplom(countries2012, "World Development", "Indicator Correlations", FALSE);
#
#
#
fathers <- read.table("Doc/fathers.txt");
Barpercent(fathers$NOHSDEG, fathers$AGE);
mtext(paste("Fathers without High School Degrees: \n", "Age at First Child"),
	  side = 3,
	  line = 1,
	  font = 2,
	  cex = 1.5);
#
#"Fathers without High School Degrees:\n"
#"Age at First Child"
fourColors <- c("lightblue", "skyblue3", "rosybrown1", "rosybrown3");
DataFrameToPieChart(fathers, fourColors,
					"Fathers without High School Degrees:\n", "Age at First Child");
#
#
#
columns <- c("NOHSDEG", "HSDEG", "SOMECOLL", "COLLDEG");
aMatrix <- as.matrix(subset(fathers, select = columns));
eduGroups <- c("No H.S. degree", "H.S. degree", "Some college", "College degree");
#"Father's Age at First Child, by Education"
MatrixToBarchartGroup(aMatrix, eduGroups, fourColors, "Father's Age at First Child, by Education");
#
#
#
columns <- c("COLLDEG", "SOMECOLL", "HSDEG","NOHSDEG");
aMatrix <- as.matrix(subset(fathers, select = columns));
eduGroups <- c("College degree", "Some college", "H.S. degree", "No H.S. degree");
MatrixToHorizontalBarchart(aMatrix, eduGroups, fourColors, fathers$AGE, "Father's Age at First Child, by Education");
#
#"2010-2014 American Community"
#"Survey: State Age and Income"
acs2014 <- read.csv("Doc/acs2014.csv", row.names = 1);
acs2014 <- acs2014[order(acs2014$Income),];
panelDesc <- data.frame(
  type = c("map", "id", "dot", "dot"),
  lab1 = c("", "", "Median Age", "Median Income"),
  lab3 = c("", "", "", "in 2014 inflation-adjusted $"),
  col1 = c(NA, NA, "Age", "Income"));
MicromapPlot(acs2014, panelDesc, "full", "Age", FALSE,
			 "2010-2014 American Community", "Survey: State Age and Income");
#
#
#
livingDataFrame <- read.csv("Doc/living.csv");
twoColors <- c("lightblue", "skyblue3");
livingLevelList <- c("Other group quarters", "Nursing home", "Alone in household", "With others in household");
livingDataFrame$LIVING <- factor(livingDataFrame$LIVING, levels = livingLevelList);

twoLineLables <- c("Other group\nquarters", "Nursing home", "Alone in\nhousehold", "With others\nin household");
ageLevelList <- c("70-79 yrs", "80-89 yrs", "90-99 yrs", "100+ yrs");
livingDataFrame$AGE <- factor(livingDataFrame$AGE, levels = ageLevelList);
LivingToGgplot(livingDataFrame, twoLineLables, twoColors, "Living Arrangements of Older Age-Sex Groups");
#
#
#
factorList <- c("legend.text =", "element_text", "(family =", "Courier)");
assign("legend.title=element_text\n(family=\"Times\")", factor(factorList));
zFactors <- c("strip.text.x = element_", "strip.text.x = element_", "text(color = \"blue\")", "text(color = \"blue\")");
wFactors <- c("strip.text.y = element_", "strip.text.y = element_", "text(color = \"red\")", "text(color = \"red\")");

dataFrame <- data.frame(x = c(1, 2, 3, 4),
				 y = c(1, 2, 3, 4),
							 z = factor(zFactors),
										 w = factor(wFactors),
													 `legend.title=element_text\n(family="Times")`);

fourColors <- c("lightblue", "skyblue3", "rosybrown1", "rosybrown3");
title <- paste0("plot.title = element_text\n", "(color = \"darkgreen\",size = rel(1.5))");
xLabels <- rep("axis.text.x", 2);
yLabels <- rep("axis.text.y", 2);
limitList <- c("legend.text =", "element_text", "(family =", "Courier)");
#DataFrameThemedPlot(dataFrame);
#
#
#
Question <- c("Are a waste of time",
			  "Help develop good\nproblem solving skills",
			  "Promote teamwork and\ncommunication",
			  "Are a better form of\nentertainment than TV");
dataFrame <- data.frame("not true for\nmost games" = c(24,16,23,30),
						"unsure" = c(16,20,28,24),
						"true for some games,\nbut not others" = c(33,47,37,34),
						"true for\nmost games" = c(26,17,10,11),
						Question,
						check.names = FALSE);
QuestionToLikert(Question, dataFrame,
				 "Public Attitudes Toward Video Games",
				 "% of all adults who think the above qualities are...");
#"Living Arrangments of Older Age-Sex Groups"
#"percent"
#""
dataFrame <- read.csv("Doc/living.csv");
newlevels <- c("Other group quarters",
			   "Nursing home",
			   "Alone in household",
			   "With others in household");
ldataFrame <- spread(dataFrame, key = LIVING, value = PERCENT) %>% mutate(GROUP = paste(AGE,SEX));
aFormula <- GROUP~.;
# DataFrameToLikert <- function(aFormula,
#                               ldataFrame,
#                               TRUE,
#                               "Living Arrangments of Older Age-Sex Groups",
#                               "percent",
#                               " ");
#"Father's Age at First Child, by Education"
#"percent"
fathers <- read.table("Doc/fathers.txt");
fathersData <- gather(fathers, key = EDUCATION, value = PERCENT, -AGE);
levels(fathersData$EDUCATION) <-
  c("No H.S. degree", "H.S. degree", "Some college", "College degree");
PanelPercent(fathersData, "Father's Age at First Child, by Education","percent");
#
#
#
ans <- anscombe;
ans$id <- as.numeric(rownames(anscombe));
ans <- ans %>% gather(key,value,-id) %>%
  extract (key,c("var","group"),"(.)(.)") %>%
  spread(var,value);
ans$group <- factor(ans$group);
ans <- ans %>% arrange(group);
AnscombeQuarter(ans);
#"Total Fertility Rate vs. GDP, 2012"
#"GDP per capita (in 1000s $US)"
#"average births per woman"
countries2012 <- read.csv ("Doc/countries2012.csv");
sixColors <- c("orange","cyan","blue","green","black", "red");
ColorScatterplot(countries2012,
				 sixColors,
				 "Total Fertility Rate vs. GDP, 2012",
				 "GDP per capita (in 1000s $US)",
				 "average births per woman");
#
#
#
colors4 <- c("lightblue", "skyblue3", "rosybrown1","rosybrown3");
FathersToHorizontalBar();

