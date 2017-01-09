#' Title
#'
#' @param numeriX 
#' @param labels 
#' @param groups 
#' @param gdata 
#' @param cex 
#' @param pch 
#' @param gpch 
#' @param bg 
#' @param color 
#' @param gcolor 
#' @param lcolor 
#' @param xlim 
#' @param main 
#' @param xlab 
#' @param ylab 
#' @param adj 
#' @param ... 
NumericToChartSolid <- function(numeriX, labels = NULL, groups = NULL,
								gdata = NULL, cex = par("cex"), pch = 21,
								gpch = 21, bg = par("bg"), color = par("fg"),
								gcolor = par("fg"), lcolor = "gray",
								xlim = range(numeriX[is.finite(numeriX)]),
								main = NULL, xlab = NULL, ylab = NULL, adj = 0,...){
  if (!is.numeric(numeriX))
	stop("'numeriX' must be a numeric vector or matrix");
  
  n <- length(numeriX);
  opar <- par("mai", "mar", "cex", "yaxs");
  on.exit(par(opar));
  
  par(cex = cex, yaxs = "i");
  
  if (is.matrix(numeriX)) {
	if (is.null(labels))
	  labels <- rownames(numeriX);
	if (is.null(labels))
	  labels <- as.character(1L:nrow(numeriX));
	labels <- rep_len(labels, n);
	if (is.null(groups))
	  groups <- col(numeriX, as.factor = TRUE);
	glabels <- levels(groups);
  } else {
	if (is.null(labels))
	  labels <- names(numeriX);
	glabels <- if (!is.null(groups))
	  levels(groups)
	if (!is.vector(numeriX)) {
	  warning("'numeriX' is neither a vector nor a matrix: using as.numeric(x)");
	  numeriX <- as.numeric(numeriX);
	}
  }
  
  plot.new();
  
  linch <- if (!is.null(labels))
	max(strwidth(labels, "inch"), na.rm = TRUE)
  else 0;
  
  if (is.null(glabels)) {
	ginch <- 0;
	goffset <- 0;
  }
  else {
	ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE);
	goffset <- 0.4;
  }
  
  if (!(is.null(labels) && is.null(glabels))) {
	nmai <- par("mai");
	nmai[2L] <- nmai[4L] + max(linch + goffset, ginch) + 0.1;
	par(mai = nmai);
  }
  
  if (is.null(groups)) {
	o <- 1L:n;
	y <- o;
	ylim <- c(0, n + 1);
  }
  else {
	o <- sort.list(as.numeric(groups), decreasing = TRUE);
	numeriX <- numeriX[o];
	groups <- groups[o];
	color <- rep_len(color, length(groups))[o];
	lcolor <- rep_len(lcolor, length(groups))[o];
	offset <- cumsum(c(0, diff(as.numeric(groups)) != 0));
	y <- 1L:n + 2 * offset;
	ylim <- range(0, y + 2);
  }
  
  plot.window(xlim = xlim, ylim = ylim, log = "");
  
  lheight <- par("csi");
  
  if (!is.null(labels)) {
	linch <- max(strwidth(labels, "inch"), na.rm = TRUE);
	if (adj == 0) {
	  loffset <- (linch + 0.1)/lheight;
	}
	else {
	  loffset <- max(numeriX) / 10;
	}
	labs <- labels[o];
	mtext(labs, side = 2, line = loffset,
		  at = y, adj = adj, col = color,
		  las = 2, cex = cex, ...);
  }
  
  abline(h = y, lty = "solid", col = lcolor);
  points(numeriX, y, pch = pch, col = color, bg = bg);
  
  if (!is.null(groups)) {
	gpos <- rev(cumsum(rev(tapply(groups, groups, length)) + 2) - 1);
	ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE);
	goffset <- (max(linch + 0.2, ginch, na.rm = TRUE) + 0.1)/lheight;
	mtext(glabels, side = 2, line = goffset,
		  at = gpos, adj = 0, col = gcolor,
		  las = 2, cex = cex, ...);
	if (!is.null(gdata)) {
	  abline(h = gpos, lty = "solid");
	  points(gdata, gpos, pch = gpch, col = gcolor, bg = bg, ...);
	}
  }
  
  axis(1);
  box();
  title(main = main, xlab = xlab, ylab = ylab, ...);
  invisible();
}

#' Title VectorToHorizontalBarplot
#'
#' @param aVector 
#' @param mainTitle 
#' @param xTitle 
#' @examples VectorToHorizontalBarplot(TFR,"Fertility Rate 2012","average births per woman")
VectorToHorizontalBarplot <- function(aVector, mainTitle="", xTitle=""){
  ## ----barplot, fig.width = 7, fig.height = 4----
  if (!is.numeric(aVector))
	stop("'aVector' must be a numeric vector or matrix");
  
  barColour <- "lightblue";
  lineColour <- "grey90";
  lineMin <- round(floor(min(aVector)),0);
  lineMax <- round(ceiling(max(aVector)),0);
  
  par(mar=c(4,8,4,2));
  
  barplot(aVector,
		   horiz = TRUE,
		   col = barColour,
		   border = barColour,
		   main = mainTitle,
		   xlab = xTitle,
		   xlim = c(0,lineMax),
		   cex.lab = 1.4,
		   cex.main = 1.7,
		   cex.names = 1.4,
		   las = 1);
  
  abline(v = lineMin:lineMax, col = lineColour);
}

#' Title BoxplotWithFormula
#'
#' @param formula 
#' @param dataFrame 
#' @param isHorizontal 
#' @param minInt 
#' @param maxInt 
#' @param mainTitle 
#' @param xTitle 
#' @param lineTitle 
BoxplotWithFormula<- function(formula,
							  dataFrame = data.frame(NULL),
							  isHorizontal = TRUE,
							  minInt = 0,
							  maxInt = 100,
							  mainTitle = "",
							  xTitle = "",
							  lineTitle = ""){
  if (!is.data.frame(dataFrame))
	stop("'dataFrame' must be data.frame");
  
  barColour <- "lightblue";
  lineColour <- "grey95";
  redColour <- "red";
  par(mar = c(5,9,4,2));
  
  boxplot(formula, dataFrame, horizontal = isHorizontal,
		  ylim = c(minInt, maxInt),
		  col = barColour,
		  main = mainTitle,
		  xlab = xTitle,
		  cex.main = 1.7,
		  cex.lab = 1.4,
		  cex.axis = 1.4,
		  las = 1);
  abline (v = minInt:maxInt, col = lineColour);
  abline (v = 2, col = redColour);
  text (x = 2, y = 0.5, lineTitle, col = redColour, pos = 4);
}

VectorToHistogram <- function(numeriX, mainTitle = "", xTitle = "", withSteps = FALSE) {

	aColour <- "lightblue";
	lineMin <- round(floor(min(numeriX)), 0);
	lineMax <- round(ceiling(max(numeriX)), 0);

	if (!withSteps) {
		myBreaks <- seq(from = lineMin, to = lineMax);
		myYlimit <- c(0 * lineMin, 10 * lineMax); #80),
	} else {
		mySteps <- 0.5;
		myBreaks <- seq(from = lineMin, to = lineMax, by = mySteps);
		myYlimit <- c(0 * lineMin, 100 * mySteps);
	}

	hist(numeriX,
		breaks = myBreaks,
		col = aColour,
		main = mainTitle,
		xlab = xTitle,
		xlim = c(lineMin, lineMax),
		ylim = myYlimit,
		cex.main = 1.7,
		cex.lab = 1.4,
		las = 1);

}

DataFrame2ListToLinechart <- function(aDataFrame, mainTitle = "", xTitle = "", yTitle = "") {

	plot(aDataFrame$List1,
		aDataFrame$List2,
		type = "l",
		main = mainTitle,
		xlab = xTitle,
		ylab = yTitle,
		las = 1,
		cex.axis = 1.2,
		cex.lab = 1.4,
		cex.main = 1.7);

}

TimeSerieToLinets <- function(aTimeSerie, mainTitle="", yTitle="", xTitle="") {

	plot(aTimeSerie,
		main = mainTitle,
		ylab = yTitle,
		xlab = xTitle,
		las = 1,
		cex.main = 1.4,
		cex.lab = 1.2,
		cex.axis = 1.2);

}

TimeSerieToMonthplot <- function(aTimeSerie, mainTitle="", yTitle="", texte="") {

	## ----month-plot, fig.width = 8.5, fig.height = 4.5----
	# Month plot
	par(mar = c(5, 6, 2, 2));

	monthplot(UKDriverDeaths,
			main = mainTitle,
			labels = month.abb,
			las = 1,
			ylab = yTitle,
			cex.axis = 1.2,
			cex.main = 1.4);

	mtext(texte, side = 2, line = 4, cex = 1.3);

}

Barpercent <- function(x, names = NULL) {
	## ----barpercent, eval = TRUE-------------------------------
	if (is.null(names))
		names <- names(x);

	barColour <- "lightblue";

	par(las = 1,
		mar = c(5, 10, 4, 6),
		xpd = NA,
		xaxs = "i",
		yaxs = "i");

	data <- x[order(x)] * 100 / sum(x);
	labels <- paste(names[order(x)], "   ", formatC(data, digits = 1, format = "f", width = 4));
	xmax <- round(max(data), digits = -1);
	ymax <- (length(x) * 1.2) + 0.1;

	plot(NULL,
		xlim = c(0, xmax),
		ylim = c(0, ymax),
		axes = FALSE,
		xlab = "percent",
		ylab = "");

	barplot(data,
			horiz = TRUE,
			names.arg = labels,
			col = barColour,
			border = barColour,
			axes = FALSE,
			add = TRUE);

	axis(1, pos = 0, xlim = c(0, xmax), at = c(0, 1:(xmax / 10) * 10));

	hat <- -xmax / 16.66667;

	text(hat, 0.2, "100.0", adj = c(1, 1));
	text(hat, 0.2, "%", adj = c(0, 1));
	lines(c(0.3, 3.2) * hat, c(0.25, 0.25));
}

