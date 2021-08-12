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
      x <- z * (pi^(0.5)) / 2
      c <- array(NA, dim = 10)
      c[1] <- 1
      for (i in 2:10) {
            current <- 0
            k <- i - 1
            for (j in 1:k) {
                  m <- j-1
                  top <- c[m+1] * c[k-m]
                  bottom <- (m+1)*(2*m+1)
                  current <- current + (top/bottom)
            }
            c[i] <- current
      }
      su <- 0
      for (i in 1:10) {
            k <- i - 1
            right <- x^(2*k+1)
            den <- 2 * k + 1
            su <- su + (right * c[i] / den)
      }
      erfinv <- su * 2 / (pi^0.5)
      return(erfinv)
}