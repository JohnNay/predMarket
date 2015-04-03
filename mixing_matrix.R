###
# From http://www.babelgraph.org/wp/?p=351
###

# calculate the mixing matrix of in igraph graph object 'mygraph', by some vertex attribute 'attrib'
# can change the default use.density=FALSE to return a matrix with raw number of edges rather than density



mixmat <- function(mygraph, attrib, use.density=TRUE) {
  
  # get unique list of characteristics of the attribute
  attlist <- sort(unique(get.vertex.attribute(mygraph,attrib)))
  
  numatts <- length(attlist)

  # build an empty mixing matrix by attribute
  mm <- matrix(nrow=numatts, 
               ncol=numatts,
               dimnames=list(attlist, attlist))
  
  # calculate edge density for each matrix entry by pairing type
  # lends itself to parallel if available
  el <- get.edgelist(mygraph,names=FALSE)
  for (i in 1:numatts) {
    for (j in 1:numatts) {
      mm[i,j] <- length(which(apply(el,1,function(x) { 
          get.vertex.attribute(mygraph, attrib, x[1] ) == attlist[i] && 
            get.vertex.attribute(mygraph, attrib, x[2] ) == attlist[j]  } )))
    }  
  }
  
  # convert to proportional mixing matrix if desired (ie by edge density)
  if (use.density) mm/ecount(mygraph) else mm
}
