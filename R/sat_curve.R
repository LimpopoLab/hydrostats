#' Slope of the saturation vapor pressure curve as a function of temperature
#' 
#' This function returns the slope of the 
#' saturation vapor pressure of water in the 
#' atmosphere.  The output is in Pascals per
#' Kelvin.  This is typically represented in 
#' equations as \Delta
#' 
#' @param temp temperature
#' @param unit temperature unit (C)elcius, (K)elvin, (F)ahrenheit, (R)ankine
#' @return slope of the saturation vapor pressure curve (Pa/K)
#' @export
sat_curve <- function(temp, unit = "C") {
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
            sat_curve <- array(NA, dim = length(t))
            for (i in 1:length(t)) {
                  est <- hydrostats::e_star(t[i], "K") # this returns e*, the saturation vapor pressure from hydrostats::e_star, returns in Pa
                  t_r <- 1 - (373.15/t[i]) # internal reference for t_r, below
                  sat_curve[i] <- 100 * (3.7315 * est / (t[i]^2)) * (13.3185 - (3.952 * t_r) - (1.9335 * (t_r^2)) - (0.5196 * (t_r^3)))
            }
            return(sat_curve)
      }
}