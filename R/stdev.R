#' Standard Deviation
#' 
#' This function returns the standard deviation of the 
#' data.  This function is writen to remove NAs and 
#' use the bias-corrected statistics with (n-1) in the 
#' demominator.  The population standard deviation can 
#' be found by forcing the denominator to be (n).  Set 
#' the option to FALSE.  
#' 
#' @param x data
#' @param biascorr use the bias-corrected equation
#' @return s
#' @export
stdev <- function(x, biascorr=TRUE) {
      m <- mean(x, na.rm = TRUE)
      nas <- which(is.na(x))
      n <- length(x) - length(nas)
      if (biascorr) {
            stdev <- (sum((x-m)^2, na.rm = TRUE)/(n-1))^0.5
      } else {
            stdev <- (sum((x-m)^2, na.rm = TRUE)/(n))^0.5
      }
      return(stdev)
}