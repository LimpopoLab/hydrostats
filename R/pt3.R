#' Pearson type III distribution
#' 
#' This function returns the inverse CDF of the Pearson 
#' type III distribution, that is, the K value.  It is 
#' calculated based on the approximation of:
#' Kite (1977), Frequency and Risk Analysis in Hydrology, 
#' Water Resouces Publications, Fourt Collins.
#' as cited in:
#' Chow, Maidment, and Mays (1988), Applied Hydrology, ch. 12
#' 
#' @param c skewness of sample
#' @param Fx non-exceedence probability, F(x)=
#' @return the inverse CDF or K value
#' @export
pt3 <- function(c,Fx) {
      k <- c/6
      z <- pnorminv(Fx)
      K_T <- z + ((z^2) - 1) * k + ((z^3) -(6*z)) * (k^2) / 3 - ((z^2) - 1) * (k^3) + z * (k^4) + (k^5)/3
      pt3 <- K_T
      return(pt3)
}