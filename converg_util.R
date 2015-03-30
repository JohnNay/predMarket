
# A measure of the societal utility from convergence. 

# From the graph we get a distribution of the approximate models. 
# From this distribution, given the specified distance matrix and the choice of
# the true model, we get a distribution of the distances to the true model. The
# societal utility of a distance distribution  is assumed to be
# a classical quadratic VnM utility function of the distances. For a distribution
# of distances D, the chosen utility function is of the form

#     U(D) = alpha E(D) - beta/2 [Var(D) + (E(D))^2]

# for some alpha > beta > 0 and alpha/beta > D. This corresponds to the 
# expected utility associated with the following utility function over outcomes D_i

#     U(D_i) = alpha D_i - beta/2 D_i^2

# The values of the utility function are rather arbitrary (in part because of the)
# the arbitrariness of the distance matrix). Some properties are worth noticing yet :
#
# 1) The zero of the utility function corresponds to the situation in which every
#     one has the right approximate model
# 2) The preferences over distance distribution associated with the utility 
#     function are an extension of the 
#     Second order stochastic dominance preorder. This means that whenever a
#     distribution of distances F second order stochastic dominates another 
#     distribution G, U(F) > U(G) (this can be seen from the fact that, given the
#     constraints on the parameters, the underlying utility function over outcomes
#     is concave.). This is also true for First
#     order stochastic dominance (U(D_i) is increasing).
# 3) Given the constraints on the paramters, we also have 
#     a) U is increasing in E(D) 
#     b) U is decreasing in Var(D)


converg.util <- function (g,
                          alpha = 1.5,
                          beta  = 1){
  
  ######
  ## Argument check
  ######
  
  if (alpha < 0)
    stop("alpha cannot be negative.")
  
  if (beta < 0)
    stop("beta cannot be negative.")
  
  if (alpha < beta)
    stop("alpha cannot be smaller than beta.")
  
  if (alpha/beta < 0)
    stop("alpha/beta must be larger than any possible distance between approximate
         models. Here alpha/beta must be larger than 0.")
  
  ######
  ## Useful variables
  ######

  n.traders <- length(V(g))  
  
  ## Distances to true model
  
  V(g)$dist <- g$dist[V(g)$approx]

  ## Return utility
  
  alpha*mean(V(g)$dist) - (beta/2)*((mean(V(g)$dist))^2 + var(V(g)$dist))
  
  ##
  
  
}