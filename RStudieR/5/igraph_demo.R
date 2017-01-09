# iGraph first released in January 30, 2006 (http://igraph.org/r/news.html)
library(igraph)
demo(package="igraph")

library(rjson)
library(RCurl)
library(igraph)

githubFollowers = function (userToPlot)
{
  user_relations=NULL
  u <- paste('https://api.github.com/users/', userToPlot, '/followers', sep='')
  print(u)
  o <- fromJSON(getURL(u, httpheader = c(Accept = "application/json", 'User-Agent'="R Curl")))
  df <- as.data.frame(t(sapply(o, unlist)))
  df$user <- userToPlot
  
  if(nrow(df) > 1){ 
    user_relations <- data.frame(from=df$login, to=df$user)
  }
  print(paste(userToPlot,' ',nrow(user_relations)))
  user_relations
}

githubGraph = function (userToPlot)
{
  relations <- githubFollowers(userToPlot)
  
  for (i in 1:nrow(relations)) {
    follower <- as.character(relations[i,]$from)
    follower_relations <- githubFollowers(follower)
    
    if(!is.null(follower_relations)){ 
      relations <- merge(relations, follower_relations, all=T)
    }
  }
  
  relations
}


# Create a data.frame with "from" and "to" columns
relations = githubGraph('EzGraphs')

g <- graph.data.frame(relations, directed = T)

V(g)$label <- V(g)$name

plot(g)


# Requires rgl (3D visualization rendering)
rglplot(g, 
        layout=layout.fruchterman.reingold(g, dim=3), 
        vertex.color="green", 
        vertex.label.color='white',
        vertex.label.dist=0.4, 
        vertex.size=3)


#tkplot(g)

