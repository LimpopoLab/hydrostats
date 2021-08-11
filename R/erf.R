#' Error Function
#' 
#' This function returns the error function.  The complementary 
#' error function can be obtained by subtracting the result 
#' from one: (1-erf(t)).  This uses a truncated infinite series.
#' 
#' @param t argument
#' @return error function
#' @export
erf <- function(t) {
      su <- 0
      top <- 0
      bottom <- 0
      for (i in 1:21) {
            k <- i - 1
            top <- ((-1)^k) * z^(2*k+1)
            bottom <- factorial(k) * (2*k+1)
            su <- su + (top / bottom)
      }
      erfinv <- (2 / (pi^(0.5))) * su
      return(erfinv)
}