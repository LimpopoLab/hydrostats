#' Converts to hydrologic year from date
#' 
#' Judges and converts to hydrologic calendar from date.
#' Note that the hydrologic year starts 01 Oct in the 
#' Northern hemisphere and 01 Aug in the Southern and 
#' tropical regions.  Here, the hydrologic year is 
#' determined by the calendar year in which the 
#' hydrologic year ends.
#' 
#' @param d date in year, month, day, POSIX, or lubridate format
#' @param h "N"orthern or "S"outhern/tropical region
#' @return hydrologic year
#' @export
hyd.yr <- function(dt, h="N") {
      if ((h != "N") & (h != "S")) {
            print("Enter a valid location: N or S")
      } else {
            if (h == "N") {
                  startmonth <- 10
            } else if (h == "S") {
                  startmonth <- 8
            }
            dt <- as.Date(dt, origin = "1970-01-01")
            m <- as.numeric(format(dt, "%m"))
            y <- as.numeric(format(dt, "%Y"))
            if (m >= startmonth) {
                  hyd.yr <- y + 1
            } else {
                  hyd.yr <- y
            }
      return(hyd.yr)
      }
}
