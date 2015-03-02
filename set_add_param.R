SetAddParam <- function ( g = net,
                          burn.in = 100,
                          horizon = 50,
                          n.seq    = 1,
                          market.struct = "CDA"
  ){
  
  g <- set.graph.attribute(g,"burn.in",burn.in)
  
  g <- set.graph.attribute(g,"horizon",horizon)
  
  g <- set.graph.attribute(g,"market.struct",market.struct)
  
  g <- set.graph.attribute(g,"n.seq",n.seq)
  
  return(g)
  
}