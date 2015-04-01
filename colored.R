Colored <- function(
  g = net,
  attri = "approx"
){
  V(g)$color<- get.vertex.attribute(g,attri)
  
  d <- length(unique(V(g)$color))
  
  col <- sample(colours(), d, replace = FALSE)
  g <- set.graph.attribute(g,"colo",col)
  
  for (i in 1:d){
    c = "i"
    V(g)$color=gsub(c,col[i],V(g)$color) 
  }
  
  g
}
