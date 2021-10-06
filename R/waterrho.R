#' Density as a function of temperature
#' 
#' This function is valid for liquid water at
#' atmospheric pressure; it does not accout
#' for changes in atmospheric pressure.  Given
#' the data from USGS.  Input temperature is 
#' provided in Celcius, but can be adjusted to 
#' F, K, or R.  The output is in kg/m^3.
#' 
#' @param temp temperature
#' @param unit temperature unit (C)elcius, (K)elvin, (F)ahrenheit, (R)ankine
#' @return water density (kg/m^3)
#' @export
waterrho <- function(temp, unit = "C") {
      if (unit == "C") {
            t <- temp
      } else if (unit == "K") {
            t <- temp - 273.15
      } else if (unit == "F") {
            t <- 5*(temp-32)/9
      } else if (unit == "R") {
            temp <- temp - 459.67
            t <- 5*(temp-32)/9
      } else {
            print("Please select a valid unit.")
      }
      if (exists("t")) {
            waterrho <- array(NA, dim = length(t))
            for (i in 1:length(t)) {
                  waterrho[i] <- (1.6419e-05)*t[i]^3 - (6.0294e-03)*t[i]^2 + (2.6174e-02)*t[i] + (9.9998e+02)
            }
            return(waterrho)
      }
}