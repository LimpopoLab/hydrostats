#' Psychrometric constant as a function of temperature
#' 
#' This function returns the psychrometric 
#' constant.  The output is in Pascals per
#' Kelvin.  This is typically represented in 
#' equations as \gamma
#' 
#' @param temp temperature
#' @param unit temperature unit (C)elcius, (K)elvin, (F)ahrenheit, (R)ankine
#' @return psychrometric constant (Pa/K)
#' @export
psych <- function(temp, unit = "C") {
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
      if (exists("t")) {
            psych <- array(NA, dim = length(t))
            for (i in 1:length(t)) {
                  psych[i] <- 100 * (4.9289e-7 * (t[i]^2) + 3.4717e-4 * t[i] + 0.51443)
            }
            return(psych)
      }
}