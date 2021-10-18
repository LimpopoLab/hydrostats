#' Saturation vapor pressure as a function of temperature
#' 
#' This function returns the saturation vapor 
#' pressure of water in the atmosphere using the 
#' nested form of the Goff-Gratch formulation as 
#' derived by Lowe (1977).  Input temperature is 
#' provided in Celcius, but can be adjusted to 
#' F, K, or R.  The output is in Pascals.
#' 
#' @param temp temperature
#' @param unit temperature unit (C)elcius, (K)elvin, (F)ahrenheit, (R)ankine
#' @return saturation vapor pressure (Pa)
#' @export
e_star <- function(temp, unit = "C") {
      if (unit == "C") {
            t <- temp + 273.15 # ensure t for calculation is in Kelvin
      } else if (unit == "K") {
            t <- temp
      } else if (unit == "F") {
            t <- 273.15 + 5*(temp-32)/9
      } else if (unit == "R") {
            temp <- temp - 459.67
            t <- 273.15 + 5*(temp-32)/9
      } else {
            print("Please select a valid unit.")
      }
      a0 <- 6984.505294
      a1 <- -188.9039310
      a2 <- 2.133357675
      a3 <- -1.288580973e-2
      a4 <- 4.393587233e-5
      a5 <- -8.023923082e-8
      a6 <- 6.136820929e-11
      if (exists("t")) {
            e_star <- array(NA, dim = length(t))
            for (i in 1:length(t)) {
                  e_star[i] <- (a0 + t *(a1 + t *(a2 + t *(a3 + t *(a4 + t *(a5 + t*a6)))))) * 100
            }
            return(e_star)
      }
}