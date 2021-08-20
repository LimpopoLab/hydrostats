#' Weibull Distrubtion
#' 
#' This function returns the inverse cumulative distribution function 
#' value for the Weibull Distribution, or the third asymptote of the 
#' extreme value distribution (type III).  It is calculated based on the 
#' inverse complemenary error function.  The maximum CDF value that 
#' can be inverted is x = 0.999.
#' 
#' @param p probability of nonexceedence < 1
#' @param m mean of sample
#' @param s standard deviation of sample
#' @return value of nonexceedence for that probability
#' @export
wei <- function(p,m=0,s=1) {
      print("under construction...")
      wei <- 1
      return(wei)
}