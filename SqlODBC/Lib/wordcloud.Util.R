library(tm);
library(wordcloud);
#
#
#
WordcloudToPng <- function(dataFrameSortDesc=NULL, pngFilePath=""){
  # graph
  png(filename = pngFilePath, width = 800, height = 800);
  wordcloud(words = dataFrameSortDesc$word,
            freq = dataFrameSortDesc$freq,
            min.freq = 1,
            max.words = 200,
            random.order = FALSE,
            rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"));
  dev.off();
}
#
#
#