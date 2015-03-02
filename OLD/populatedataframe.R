#' @title  Populate Prediction Market Agent-Based model
#'   
#' @description Adapts http://www.babelgraph.org/wp/?p=237's cohortC function to populate
#'  a prediction market ABM
#'   
#' @param ntraders integer indicating the number of traders
#'   
#'   
#' @param nColleges integer indicating the number of colleges (in the college 
#'   admissions problem) or women (in the stable marriage problem) in the 
#'   market. Defaults to \code{ncol(c.decl)}.
#' @param nSlots vector of length \code{nColleges} indicating the number of 
#'   places (i.e. quota) of each college. Defaults to \code{rep(1,nColleges)} 
#'   for the marriage problem.
#' @param s.decl matrix of dimension \code{nColleges} \code{x} \code{nStudents}
#'   with the \code{i}th column containing student \code{i}'s ranking over 
#'   colleges in decreasing order of preference (i.e. most preferred first).
#' @param c.decl matrix of dimension \code{nStudents} \code{x} \code{nColleges}
#'   with the \code{j}th column containing college \code{j}'s ranking over 
#'   students in decreasing order of preference (i.e. most preferred first).



# create separate vectors to build data frame at the end

populate <- function( 
                        ntraders   = 100,
                        numbsecu   = 10, 
                      ) 
  {  
  
  # define some factors
  levels.behav  <- factor(c('ZI')) 
#   levels.flavor <- factor(c('chocolate','vanilla','strawberry','mint'))  
  
  # initialize vectors by ntraders 
  p.id    <- numeric(ntraders) 
  p.money <- numeric(ntraders) 
  p.behav <- character(ntraders) 
  p.secu  <- 
#   p.fav.flavor <- character(ntraders)   
  
  for (cc in 1:ntraders) {
    
    # generate attribute for each person in cohort 
    p.id[cc] <- cc 
    p.age[cc] <- rnorm(mean=50,sd=10,n=1)
    p.gender[cc] <- as.vector(sample(levels.behav,ntraders=1)) 
    p.fav.flavor[cc] <- as.vector(sample(levels.flavor,ntraders=1)) }   
  
  # construct data frame from vectors 
  
  df <- data.frame(id=p.id, age=p.age, gender=p.gender, fav.flavor=p.fav.flavor)   
  
  return(df)  } 


