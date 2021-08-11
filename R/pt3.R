#' Pearson type III distribution
#' 
#' This function returns the cumulative distribution function value for 
#' the Pearson type III distribution, that is, the K value.  It is 
#' calculated with the gamma function based on the skewness as an input.
#' 
#' @param m mean of sample
#' @param s standard deviation of sample
#' @param c skewness of sample
#' @return the CDF or K value
#' @export
pt3 <- function(m,s,c) {
      beta <- (2/c)^2
      lambda <- s/(beta^(0.5))
      epsilon <- m-(s*(beta^(0.5)))
      
}