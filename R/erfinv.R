#' Inverse Error Function
#' 
#' This function returns the inverse error function.  The 
#' inverse complementary error function can be obtained 
#' with the arguement of (1-z).  This uses a truncated 
#' infinite series.  If more precision is needed, re-write 
#' the series in the for loop to include more higher-order 
#' terms.
#' 
#' @param z argument
#' @return inverse error function
#' @export
erfinv <- function(z) {
      su <- 0
      top <- 0
      bottom <- 0
      for (i in 1:11) {
            k <- i - 1
            top <- z^(2*k+1)
            bottom <- factorial(k) * (2*k+1)
            su <- su + (top / bottom)
      }
      erfinv <- (2 / (pi^(0.5))) * su
      return(erfinv)
}