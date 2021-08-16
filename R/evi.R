#' Extreme Value type I
#' 
#' This function returns the inverse of the Extreme 
#' Value type I cummulative distribution F(x).  Do 
#' not normalize these data, EVI uses unique 
#' geometric statistics.  
#' 
#' @param x data
#' @param Fx Fx non-exceedence probability, F(x)=
#' @return inverse EVI CDF
#' @export
evi <- function(x,Fx) {
      s <- stdev(x)
      a <- (6^0.5) * s / pi
      m <- (mean(x, na.rm = TRUE)) - (0.5772 * a)
      y <- -log(log(1/Fx))
      evi <- (y * a) + m
      return(evi)
}