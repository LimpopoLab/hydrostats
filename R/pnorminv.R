#' Inverse Normal Cumulative Distribution Function
#' 
#' This function returns the inverse cumulative distribution function 
#' value for the normal distribution.  It is calculated based on the 
#' inverse complemenary error function.
#' 
#' @param p probability of nonexceedence < 1
#' @param m mean of sample
#' @param s standard deviation of sample
#' @return value of nonexceedence for that probability
#' @export
pnorminv <- function(p,m=0,s=1) {
      if ((p<1)&(p>0)) {
            y <- -(2^(0.5)) * erfinv(1 - (2 * p))
            pnorminv <- (y * s) + m
            return(pnorminv)
      } else {
            print("Enter a valid probability.")
      }
}