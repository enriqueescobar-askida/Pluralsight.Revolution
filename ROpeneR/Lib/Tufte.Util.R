#
#
#
TwoVectorBaseLinePlot <- function(x, y, title1, title2){
  dollarLabels <- sprintf("$%s", seq(300, 400, 20));
  plot(y ~ x, axes = F, xlab = "", ylab = "", pch = 16, type = "b");
  axis(1,
       at = x,
       label = x,
       tick = F,
       family = "serif");
  axis(2,
       at = seq(1, 6, 1),
       label = dollarLabels,
       tick = F,
       las = 2,
       family = "serif");
  abline(h = 6, lty = 2);
  abline(h = 5, lty = 2);
  text(max(x),
       min(y) * 2.5,
       title1,
       adj = 1,
       family = "serif");
  text(max(x),
       max(y) / 1.08,
       labels = title2,
       family = "serif");
}
#
#
#
library(lattice);
TwoVectorLatticeLinePlot <- function(x, y, title1, title2){
  dollarLabels <- sprintf("$%s", seq(300, 400, 20));
  xyplot(y ~ x,
         xlab = "",
         ylab = "",
         pch = 16,
         col = 1,
         border = "transparent",
         type = "o",
         abline = list(h = c(max(y), max(y) - 1), lty = 2),
         scales = list(x = list(at = x, labels = x, fontfamily = "serif", cex = 1),
                       y = list(at = seq(1, 6, 1),
                                fontfamily = "serif",
                                cex = 1,
                                label = sprintf("$%s", seq(300, 400, 20)))),
         par.settings = list(axis.line = list(col = "transparent"),
                             dot.line = list(lwd = 0)),
         axis = function(side, line.col = "black", ...){
           if (side %in% c("left", "bottom")) {
             axis.default(side = side, line.col = "black", ...)
           }
         });
  ltext(current.panel.limits()$xlim[2] / 1.1,
        adj = 1,
        fontfamily = "serif",
        current.panel.limits()$ylim[1] / 1.3,
        cex = 1,
        title1);
  ltext(current.panel.limits()$xlim[2] / 1.1,
        adj = 1,
        fontfamily = "serif",
        current.panel.limits()$ylim[1] / 5.5,
        cex = 1,
        title2);
}
#
#
#
library(ggplot2);
library(ggthemes);
TwoVectorGgplot2LinePlot <- function(x, y, title1, title2){
  dataFrame <- data.frame(x, y);
  dollarLabels <- sprintf("$%s", seq(300, 400, 20));
  ggplot(dataFrame, aes(x, y)) + geom_line() + geom_point(size = 3) +
    theme_tufte(base_size = 15) +
    theme(axis.title = element_blank()) +
    geom_hline(yintercept = c(5, 6), lty = 2) +
    scale_y_continuous(breaks = seq(1, 6, 1), label = dollarLabels) +
    scale_x_continuous(breaks = x, label = x) +
    annotate("text",
             x = c(1977, 1977.2),
             y = c(1.5, 5.5),
             adj = 1,
             family = "serif",
             label = c(title1, title2));
}
#
#
#
RangeFrameBasePlot <- function(x, y, xTitle, yTitle){
  plot(x,
       y,
       main = "",
       axes = FALSE,
       pch = 16,
       cex = 0.8,
       family = "serif",
       xlab = xTitle,
       ylab = yTitle);
  axis(1,
       at = summary(x),
       labels = round(summary(x), 1),
       tick = F,
       family = "serif");
  axis(2,
       at = summary(y),
       labels = round(summary(y), 1),
       tick = F,
       las = 2,
       family = "serif");
}
#
#
#
RangeFrameLatticePlot <- function(x, y, xTitle, yTitle){
  xyplot(y ~ x,
         mtcars,
         col = 1,
         pch = 16,
         fontfamily = "serif",
         xlab = xTitle,
         ylab = yTitle,
         par.settings = list(axis.line = list(col = "transparent"),
                             par.xlab.text = list(fontfamily = "serif"),
                             par.ylab.text = list(fontfamily = "serif")),
         scales = list(x = list(at = summary(x),
                                labels = round(summary(x), 1),
                                fontfamily = "serif"),
                       y = list(at = summary(y),
                                labels = round(summary(y), 1),
                                fontfamily = "serif")),
         axis = function(side, line.col = "black", ...){
           if (side %in% c("left", "bottom")){
             axis.default(side = side, line.col = "black", ...)
           }
         }
  )
}
#
#
#
library(ggplot2);
library(ggthemes);
RangeFrameGgplot2Plot <- function(dataFrame=NULL, xTitle, yTitle){
  ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_rangeframe() +
    theme_tufte() +
    xlab(xTitle) +
    ylab(yTitle) +
    theme(axis.title.x = element_text(vjust = -0.5),
          axis.title.y = element_text(vjust = 1.5));
}
#
#
#
library(lattice);
LatticeDotDash <- function(x, xTitle="", y, yTitle=""){
  xyplot(y ~ x,
         xlab = xTitle,
         ylab = yTitle,
         par.settings = list(axis.line = list(col = "transparent")),
         panel = function(x, y, ...) {
           panel.xyplot(x, y, col = 1,
                        pch = 16);
           panel.rug(x, y, col = 1,
                     x.units = rep("snpc", 2),
                     y.units = rep("snpc", 2), ...);
         }
  );
}
#
#
#
library(ggplot2);
library(ggthemes);
Ggplot2DotDash <- function(dataFrame=NULL, xTitle="", yTitle=""){
  ggplot(dataFrame, aes(wt, mpg)) + geom_point() + geom_rug() +
    theme_tufte(ticks = F) +
    xlab(xTitle) + ylab(yTitle) +
    theme(axis.title.x = element_text(vjust = -0.5),
          axis.title.y = element_text(vjust = 1));
}
#
#
#
BaseQuakesBoxplot <- function(dataFrame=NULL, description=""){
  x <- dataFrame$mag;
  y <- dataFrame$stations;
  boxplot(y ~ x,
          main = "",
          axes = FALSE,
          xlab = " ",
          ylab = " ",
          pars = list(boxcol = "transparent", medlty = "blank", medpch = 16,
                      whisklty = c(1, 1), medcex = 0.7, outcex = 0, staplelty = "blank"));
  axis(1,
       at = 1:length(unique(x)),
       label = sort(unique(x)),
       tick = F,
       family = "serif");
  axis(2,
       las = 2,
       tick = F,
       family = "serif");
  text(min(x) / 3,
       max(y) / 1.1,
       pos = 4,
       family = "serif",
       description);
}
#
#
#
LatticeQuakesBoxplot <- function(dataFrame=NULL, description=""){
  x <- dataFrame$mag;
  y <- dataFrame$stations;
  bwplot(y ~ x,
         horizontal = F,
         xlab = "",
         ylab = "",
         do.out = FALSE,
         box.ratio = 0,
         scales = list(x = list(labels = sort(unique(x)),
                                fontfamily = "serif"),
                       y = list(fontfamily = "serif")),
         par.settings = list(axis.line = list(col = "transparent"),
                             box.umbrella = list(lty = 1, col = 1),
                             box.dot = list(col = 1),
                             box.rectangle = list(col = c("transparent")))
  );
  ltext(current.panel.limits()$xlim[1] + 250,
        adj = 1,
        current.panel.limits()$ylim[2] + 50,
        fontfamily = "serif",
        description);
}
#
#
#
library(ggplot2);
library(ggthemes);
Ggplot2QuakesBoxplot <- function(dataFrame=NULL, description=""){
  ggplot(dataFrame, aes(factor(mag), stations)) + theme_tufte() +
    geom_tufteboxplot(outlier.colour = "transparent") +
    theme(axis.title = element_blank()) +
    annotate("text",
             x = 8,
             y = 120,
             adj = 1,
             family = "serif",
             label = c(description));
}
#
#
#
library(psych);
BaseBarchart <- function(myData, text=""){
  barplot(myData,
          xaxt = "n",
          yaxt = "n",
          ylab = "",
          border = F,
          width = c(.35),
          space = 1.8);
  axis(1,
       at = (1:length(myData)) - .26,
       labels = names(myData),
       tick = F,
       family = "serif");
  axis(2,
       at = seq(1, 5, 1),
       las = 2,
       tick = F,
       family = "serif");
  abline(h = seq(1, 5, 1),
         col = "white",
         lwd = 3);
  abline(h = 0,
         col = "gray",
         lwd = 2);
  text(min(myData) / 2,
       max(myData) / 1.2,
       pos = 4,
       family = "serif",
       text);
  
}
#
#
#

#
#
#

#
#
#