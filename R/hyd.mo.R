#' Converts to hydrologic month from date
#' 
#' Judges and converts to hydrologic calendar from date.
#' Note that the hydrologic year starts 01 Oct in the 
#' Northern hemisphere and 01 Aug in the Southern and 
#' tropical regions.
#' 
#' @param dt date in year, month, day, POSIX, or lubridate format
#' @param h "N"orthern or "S"outhern/tropical region
#' @return hydrologic year
#' @export
hyd.mo <- function(dt, h="N") {
      if ((h != "N") & (h != "S")) {
            print("Enter a valid location: N or S")
      } else {
            if (h == "N") {
                  startmonth <- 10
            } else if (h == "S") {
                  startmonth <- 7
            }
            dt <- as.Date(dt, origin = "1970-01-01")
            m <- as.numeric(format(dt, "%m"))
            if (m >= startmonth) {
                  hyd.mo <- m - (startmonth-1)
            } else {
                  hyd.mo <- m + (12-startmonth+1)
            }
            return(hyd.mo)
      }
}
