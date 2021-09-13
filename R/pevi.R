#' Extreme Value type I
#' 
#' This function returns the cummulative 
#' distribution F(x) of the Extreme Value 
#' type I .  Do not normalize these data, 
#' EVI uses unique geometric statistics.  
#' 
#' @param d data
#' @param x x random variable quantity
#' @return non-exceedence probability, F(x)=
#' @export
pevi <- function(d,x) {
      s <- stdev(d)
      a <- (6^0.5) * s / pi
      m <- (mean(d, na.rm = TRUE)) - (0.5772 * a)
      y <- (x - m) / a
      pevi <- exp(-exp(-y))
      return(pevi)
}