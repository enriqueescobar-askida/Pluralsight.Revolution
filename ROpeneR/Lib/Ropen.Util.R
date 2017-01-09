latticeLib <- paste0(c("Lib/", "import_lattice.R"), sep = "", collapse = "");
write(paste0(c("Load Lib ........\t", latticeLib), sep = "", collapse = ""), stdout());
source(latticeLib);
#
#
#
CountriesToSplom <- function(dataFrame, mainTitle="", subTitle="", selected=FALSE){
  dataFrame$GDP <- dataFrame$GDP/1000
  
  if(selected){
	splom(dataFrame[,c("GDP","TFR","LIFEEXP","CHMORT")],
		  scales = list(y = list(tick.number = 0)),
		  main = list(label = paste(mainTitle, subTitle), cex = 1.7),
		  xlab=NULL);
  } else {
	dataFrame <- droplevels(dataFrame[dataFrame$CONTINENT
							%in%
							c("Europe","Asia","Africa"),]);
	threeColors <- c("orange","cyan","blue");
	splom(dataFrame[,c("GDP","TFR","LIFEEXP","CHMORT")],
		  col = threeColors[dataFrame$CONTINENT],
		  key = list(space="right", points=list(pch = 21, col=threeColors), text=list(levels(dataFrame$CONTINENT))),
		  main = list(label = paste(mainTitle, subTitle), cex = 1.7),
		  xlab=NULL);
  }
}
#
#
#
massLib <- paste0(c("Lib/", "import_MASS.R"), sep = "", collapse = "");
write(paste0(c("Load Lib ........\t", massLib), sep = "", collapse = ""), stdout());
source(massLib);
#
#
#
CountriesToParallelPlot <- function(dataFrame, mainTitle=""){
  par(bg='white');
  sixColors <- c("orange", "cyan", "blue", "green", "black", "red");
  legendPosition <- "right";
  
  parcoord(dataFrame[,c("GDP","TFR","LIFEEXP","CHMORT")],
		   col = sixColors[dataFrame$CONTINENT],
		   xlim = c(1,5.25),
		   main = mainTitle,
		   cex.main = 1.4,
		   xaxs = "i");
  
  axis(2, at = c(0,1), labels = c("min", "max"), las = 1, tick = FALSE);
  legend(legendPosition, lty = 1, legend = levels(dataFrame$CONTINENT), col = sixColors, cex = 0.9, bty = "n");
}
#
#
#
baseLib <- paste0(c("Lib/", "base.Util.R"), sep = "", collapse = "");
write(paste0(c("Load Util........\t", baseLib), sep = "", collapse = ""), stdout());
source(baseLib);
#
#
#
CountriesDataFrameToScatterplot <- function(dataFrame, mainTitle="", xTitle="", yTitle="") {

	colour <- "cornflowerblue";

	plot(dataFrame$GDP / 1000,
		 dataFrame$TFR,
		 xpd = TRUE,
		 bty = "l",
		 main = mainTitle,
		 xlab = xTitle,
		 ylab = yTitle,
		 ylim = c(1, 8),
		 cex.main = 1.5,
		 cex.lab = 1.3,
		 cex.axis = 1.1,
		 col = colour,
		 las = 1);

}
#
#
#
CountriesDataFrameToColorScatterplot <- function(dataFrame, mainTitle = "", xTitle = "", yTitle = "") {

	sixColors <- c("orange", "cyan", "blue", "green", "black", "red");
	legendPosition <- "topright";

	plot(dataFrame$GDP / 1000,
		dataFrame$TFR,
		bty = "l",
		col = sixColors[dataFrame$CONTINENT],
		xlab = xTitle,
		ylab = yTitle,
		main = mainTitle,
		las = 1,
		ylim = c(1, 8),
		cex.main = 1.5,
		cex.lab = 1.3,
		cex.axis = 1.1);

	legend(legendPosition,
			pch = 21,
			legend = levels(dataFrame$CONTINENT),
			col = sixColors,
			bty = "n");

}

CountryFromDataFrameToColorScatterplot <- function(dataFrame, mainTitle = "", xTitle = "", yTitle = "", countryName="United States") {

	colour <- "cornflowerblue";

	plot(dataFrame$GDP / 1000,
		dataFrame$TFR,
		xpd = TRUE,
		bty = "l",
		main = mainTitle,
		xlab = xTitle,
		ylab = yTitle,
		ylim = c(1, 8),
		cex.main = 1.5,
		cex.lab = 1.3,
		cex.axis = 1.1,
		col = colour,
		las = 1);

	usdata <- dataFrame[dataFrame$COUNTRY == countryName,];
	usColour <- "magenta4";

	text(usdata$GDP / 1000,
		usdata$TFR,
		labels = usdata$COUNTRY,
		pos = 3,
		col = usColour);

	points(usdata$GDP / 1000,
		usdata$TFR,
		cex = 1.2,
		col = usColour);

}
micromapSTUtil <- paste0(c("Lib/", "micromapST.Util.R"), sep = "", collapse = "");
write(paste0(c("Load Util........\t", micromapSTUtil), sep = "", collapse = ""), stdout());
source(micromapSTUtil);
#
#
#
ggplot2Util <- paste0(c("Lib/", "ggplot2.Util.R"), sep = "", collapse = "");
write(paste0(c("Load Util........\t", ggplot2Util), sep = "", collapse = ""), stdout());
source(ggplot2Util);
#
LivingToGgplot <- function(livingDataFrame, lineLables, lineColors, mainTitle="") {

	state <- "identity";
	position <- "dodge";
	limitList <- c("Male", "Female");
	scaleTo <- "cm";
	legendStyle <- "lines";
	legendColor <- "grey90";
	titleStyle <- "bold";

	g <- ggplot(livingDataFrame, aes(x = LIVING, y = PERCENT, fill = SEX));
	g + geom_bar(stat = state, position = position) +
	scale_x_discrete(labels = lineLables) +
	scale_fill_manual(values = lineColors, limits = limitList) +
	scale_y_continuous(breaks = c(0, 30, 60)) +
	coord_flip() +
	facet_grid(. ~ AGE) +
	theme_bw(16) +
	theme(axis.ticks.length = unit(0, scaleTo),
		legend.title = element_blank(),
		legend.position = c(0.9, -0.2),
		legend.key = element_blank(),
		legend.key.size = unit(0.8, legendStyle),
		panel.grid.major.y = element_blank(),
		panel.grid.minor = element_blank(),
		strip.background = element_rect(fill = legendColor),
		strip.text.x = element_text(margin = margin(t = 5, b = 5)),
		plot.title = element_text(hjust = 0, size = 16, face = titleStyle),
	plot.margin = margin(8, 8, 24, 8)) +
	xlab(NULL) +
	ylab("percent") +
	ggtitle(mainTitle);
}
#
#
#
DataFrameThemedPlot <- function(dataFrame, xLabels, yLabels) {
	ggplot(dataFrame, aes(x, y, color = `legend.title=element_text\n(family="Times")`)) +
	geom_point(size = 3) +
	facet_grid(w ~ z) +
	scale_x_continuous(breaks = c(1, 3), labels = xLabels) +
	scale_y_continuous(breaks = c(1, 3), labels = yLabels) +
	scale_color_manual(values = fourColors, limits = limitList) +
	theme_bw(base_size = 14) +
	theme(strip.text.x = element_text(color = "blue"),
		  strip.text.y = element_text(color = "red"),
			  plot.title = element_text(color = "darkgreen", size = rel(1.5)),
				  axis.title.x = element_text(face = "italic"),
					  axis.title.y = element_text(face = "bold"),
						  legend.title = element_text(family = "Times"),
							  legend.text = element_text(family = "Courier"),
								  legend.key = element_blank()) +
	ggtitle(title) +
	xlab("axis.title.x = element_text (face = \"italic\")") +
	ylab("axis.title.y = element_text (face = \"bold\")");
}
#
#
#
hhLib <- paste0(c("Lib/", "import_HH.R"), sep = "", collapse = "");
write(paste0(c("Load Lib ........\t", hhLib), sep = "", collapse = ""), stdout());
source(hhLib);
#
#
#
QuestionToLikert <- function(Question, dataFrame, mainTitle, xTitle){
  
  likert(Question ~ .,
		 dataFrame,
		 ReferenceZero = 2,
		 positive.order = TRUE,
		 main = mainTitle,
		 xlab = xTitle,
		 ylab = "");
}
#
#
#
tidyrLib <- paste0(c("Lib/", "import_tidyr.R"), sep = "", collapse = "");
write(paste0(c("Load Lib ........\t", tidyrLib), sep = "", collapse = ""), stdout());
source(tidyrLib);
#
dplyrLib <- paste0(c("Lib/", "import_dplyr.R"), sep = "", collapse = "");
write(paste0(c("Load Lib ........\t", dplyrLib), sep = "", collapse = ""), stdout());
source(dplyrLib);
DataFrameToLikert <- function(aFormula,
                              dataFrame,
                              isPositive=TRUE,
                              mainTitle,
                              xTitle,
                              yTitle){
  
  likert(aFormula,
         dataFrame,
         positive.order = isPositive,
         main = mainTitle,
         xlab = xTitle,
         ylab = yTitle);
}
#
#
#
PanelPercent <- function(data, mainTitle, yTitle){
  g <- ggplot(data, aes(x = AGE, y = PERCENT));
  g + geom_bar(stat = "identity", fill = "lightblue") +
    coord_flip(expand = FALSE) + facet_grid(.~EDUCATION) +
  theme_bw(16) +
  theme(axis.line = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="grey90"),
        strip.text.x = element_text(margin = margin(t = 5, b= 5)),
        plot.title = element_text(face = "bold")) +
  ggtitle(mainTitle) + xlab(NULL) + ylab(yTitle);
  
}
#
#
#
AnscombeQuarter <- function(ans){
  g <- ggplot(ans,aes(x,y));
  g + geom_point() + facet_wrap(~group) +
    coord_fixed(ratio = 1.3) +
    ggtitle("Anscombe's Quartet") +
    theme_bw(16);
}
#
#
#
ColorScatterplot <- function(data, sixColors, mainTitle,
                             xTitle, yTitle){
  g <- ggplot(data, aes(GDP/1000, TFR, color = CONTINENT))
  g + geom_point(shape = 1, size =3) +
    scale_color_manual(values = sixColors) +
    theme_classic(16) +
    theme(plot.title = element_text(face = "bold"),
          legend.title = element_blank()) +
    ggtitle(mainTitle) + xlab (xTitle) + ylab (yTitle);
}
#
#
#
CountriesDatarameToBoxplot <- function(){
  g <- ggplot(data, aes(x = reorder(CONTINENT, TFR, median), y = TFR));
  g + geom_hline(yintercept = 2, color = "red") +
    geom_boxplot(fill="lightblue") +
    annotate("text", x = .75, y = 3.4,
             label = "<- replacement rate", color = "red",
             size = 5) +
    coord_flip() +
    scale_y_continuous(limits = c(1,8), breaks = 1:8) +
    theme_bw(16) +
    theme(axis.text = element_text(size = rel(1)),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold")) +
    ggtitle ("Fertility Rate Distributions by Continent") +
    xlab(NULL) + ylab("average births per woman")
}
#
#
#
FathersToHorizontalBar <- function(data, colors, maintTitle, yTitle){
  g <- ggplot(data, aes(x = EDUCATION, y = PERCENT, fill = AGE));
  g + geom_bar(stat = "identity") +
    coord_flip(expand = FALSE) +
    scale_fill_manual(values = colors) + theme_bw(16) +
    theme(panel.grid.major = element_blank(),
          panel.border = element_blank(),
          legend.position="top",
          legend.title = element_blank(),
          legend.key = element_blank(),
          axis.ticks.length = unit(0, "cm"),
          plot.title = element_text(face = "bold")) +
    ggtitle(maintTitle) +
    xlab(NULL) + ylab(yTitle);
}
#
#
#
FathersToVerticalBar <- function(data, colors, maintTitle, yTitle){
  g <- ggplot(data, aes(x = EDUCATION, y = PERCENT, fill = AGE));
  g + geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = colors) + theme_classic(16) +
    theme(axis.line = element_blank(),
          axis.ticks.length = unit (0,"cm"),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.major.x = element_blank(),
          legend.position="top",
          legend.title = element_blank(),
          plot.title = element_text(face = "bold")) +
    ggtitle (maintTitle) + xlab(NULL) + ylab(yTitle);
}
#
#
#
tufteLib <- paste0(c("Lib/", "Tufte.Util.R"), sep = "", collapse = "");
write(paste0(c("Load Lib ........\t", tufteLib), sep = "", collapse = ""), stdout());
source(tufteLib);
#
#
#