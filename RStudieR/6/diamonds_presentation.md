Diamonds Presentation
========================================================
author: 
date: 

Simplest possible chunk
========================================================
No name or options specified.

```r
  library(ggplot2)
  ggplot(diamonds, aes(x=color)) + geom_bar()
```

![plot of chunk unnamed-chunk-1](diamonds_presentation-figure/unnamed-chunk-1.png) 

Chunk with Options
========================================================


```r
  ggplot(diamonds, aes(x=color, fill=color)) + geom_bar()
```

<img src="diamonds_presentation-figure/html-diamond-bar.png" title="plot of chunk html-diamond-bar" alt="plot of chunk html-diamond-bar" style="display: block; margin: auto;" />

Inline Code
========================================================

* 1 + 3 = 4
* $pi$ = 3.1416

