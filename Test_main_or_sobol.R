# A simple test to give a better idea of whether the problem comes from main or
# from sobol et al.

source("main.R")

for ( j in 1:20){
  rm(list=ls()) 
  s <- runif(4, min = 0.0001, max = 0.9999)
  print(s)
  result <- main(parameters = s, out = "converg")
  print(result)
}
  
  
  
