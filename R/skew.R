#' Skewness or third moment about the mean
#' 
#' This function returns the skewness of the data.  This 
#' function is writen to remove NAs and use the bias-
#' corrected statistics.
#' 
#' @param x data
#' @return skewness
#' @export
skew <- function(x) {
      m <- mean(x, na.rm = TRUE)
      nas <- which(is.na(x))
      n <- length(x) - length(nas)
      s <- (sum((x-m)^2, na.rm = TRUE)/(n-1))^0.5
      m3 <- (sum((x-m)^3, na.rm = TRUE)/(n-1))
      skew <- m3/(s^3)
      return(skew)
}