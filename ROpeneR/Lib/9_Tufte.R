# requirements
#install.packages(c("CarletonStats", "devtools", "fmsb", "ggplot2", "ggthemes",
#				   "latticeExtra", "MASS", "PerformanceAnalytics", "psych",
#				   "plyr", "proto", "RCurl", "reshape", "reshape2"));

# Minimal barchart 
## Minimal barchart in lattice
library(lattice);
library(psych);
d <- colMeans(msq[, c(2, 7, 34, 36, 42, 43, 46, 55, 68)], na.rm = T) * 10;
barchart(sort(d),
        xlab = "",
        ylab = "",
        col = "grey",
        origin = 1,
        border = "transparent",
        box.ratio = 0.5,
        panel = function(x, y, ...) {
          panel.barchart(x, y, ...);
          panel.abline(v = seq(1, 6, 1), col = "white", lwd = 3);
        },
        par.settings = list(axis.line = list(col = "transparent")));
ltext(current.panel.limits()$xlim[2] - 50,
      adj = 1,
      current.panel.limits()$ylim[1] - 100,
      "Average scores\non negative emotion traits\nfrom 3896 participants\n(Watson et al., 1988)");
## Minimal barchart in ggplot2
library(ggplot2)
library(ggthemes)
library(psych)
library(reshape2)
d <- melt(colMeans(msq[, c(2, 7, 34, 36, 42, 43, 46, 55, 68)], na.rm = T) * 10);
d$trait <- rownames(d);
ggplot(d, aes(x = trait, y = value)) + theme_tufte(base_size = 14, ticks = F) +
  geom_bar(width = 0.25, fill = "gray", stat = "identity") +
  theme(axis.title = element_blank()) +
  scale_y_continuous(breaks = seq(1, 5, 1)) +
  geom_hline(yintercept = seq(1, 5, 1), col = "white", lwd = 1) +
  annotate("text", x = 3.5, y = 5, adj = 1, family = "serif",
          label = c("Average scores\non negative emotion traits
          from 3896 participants \n(Watson et al., 1988) "));
