###
# From http://www.babelgraph.org/wp/?p=351 for more details
###
# calculate the assortativity coefficient for a mixing matrix of a graph
# ref: MEJ Newman, 'Mixing patterns in networks', Phys Rev E 67, 026126 (2003)
#
# define assortativity coefficient as
#        trace (m) - sum (m^2)
# ac = -------------------------
#           1 - sum (m^2)
# 
# From Newman " This formula gives 
#                                 r = 0 when there is no assortative mixing,
#                                 r = 1 when there is perfect assortative mixing
#                                 some negative value between -1 and 0 when the
#                                 is perfectly disassortiative, i.e., every edge
#                                 connects two vertices of different types

assortcoeff <- function(m) {
        tr <- 0
        for (k in 1:nrow(m)) tr <- tr + m[k,k]
        sumsq <- sum (rowSums(m)*colSums(m))
        (tr - sumsq) / (1 - sumsq)
}