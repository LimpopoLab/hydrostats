#' Converts to hydrologic day from date
#' 
#' Judges and converts to hydrologic calendar from date.
#' Note that the hydrologic year starts 01 Oct in the 
#' Northern hemisphere and 01 Aug in the Southern and 
#' tropical regions.
#' 
#' @param d date in year, month, day, POSIX, or lubridate format
#' @param h "N"orthern or "S"outhern/tropical region
#' @return hydrologic year
#' @export
hyd.da <- function(d, h="N") {
      if ((h != "N") & (h != "S")) {
            print("Enter a valid location: N or S")
      } else {
            dt <- as.Date(d, origin = "1970-01-01")
            d <- as.numeric(dt)
            m <- as.numeric(format(dt, "%m"))
            y <- as.numeric(format(dt, "%Y"))
            if (h == "N") {
                  startmonth <- 10
            } else if (h == "S") {
                  startmonth <- 8
            }
            if (m >= startmonth) {
                  datum <- as.numeric(as.Date(paste0(y, "-", (startmonth-1), "-30"), origin = "1970-01-01"))
                  hyd.da <- as.numeric(dt) - datum # day of hydrologic year
            } else {
                  datum <- as.numeric(as.Date(paste0((y-1), "-", (startmonth-1), "-30"), origin = "1970-01-01"))
                  hyd.da <- as.numeric(dt) - datum # day of hydrologic year
            }
            return(hyd.da)
      }
}
