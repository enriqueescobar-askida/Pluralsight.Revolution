library(ggvis)
library(ggplot2)

content <- function(data){
  paste0("Carat: ", data$carat, " Price: ", as.character(data$price))
}

diamonds %>% 
  ggvis(~carat, ~price, fill=~cut) %>% 
  layer_points(opacity:=1/2) %>%
  add_tooltip(content, "hover")
