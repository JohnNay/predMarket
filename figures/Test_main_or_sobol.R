# A simple test to give a better idea of whether the problem comes from main or
# from sobol et al.

for ( j in 1:10000){
  rm(list=ls()) 
  source("main.R")
  s <- runif(4, min = 0.0001, max = 0.9999)
  result <- main(parameters = s)
  print(result)
}
  
  
  
