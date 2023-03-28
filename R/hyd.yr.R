#' Converts to hydrologic year from date
#' 
#' Judges and converts to hydrologic calendar from date.
#' Note that the hydrologic year starts 01 Oct in the 
#' Northern hemisphere and 01 Jul in the Southern and 
#' tropical regions.  Users can also dtermine the start 
#' month of their custom hydrologic year.  Here, the 
#' hydrologic year is determined by the calendar year 
#' in which the hydrologic year ends.
#' 
#' @param dt date in year, month, day, POSIX, or lubridate format
#' @param s "N"orthern or "S"outhern/tropical region, numerical start month - 01 for January, 12 for December
#' @return hydrologic year
#' @export
hyd.yr <- function(dt, s="N") {
      startmonth <- 1
      if (is.na(s)) {
            print("Enter a valid location: N or S, or start month numeral")
      } else if (is.numeric(s)) {
            startmonth <- s
      } else if ((s != "N") & (s != "S")) {
            print("Enter a valid location: N or S")
      } else {
            if (s == "N") {
                  startmonth <- 10
            } else if (s == "S") {
                  startmonth <- 7
            }
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
