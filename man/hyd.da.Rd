% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hyd.da.R
\name{hyd.da}
\alias{hyd.da}
\title{Converts to hydrologic day from date}
\usage{
hyd.da(d, h = "N")
}
\arguments{
\item{d}{date in year, month, day, POSIX, or lubridate format}

\item{h}{"N"orthern or "S"outhern/tropical region}
}
\value{
hydrologic year
}
\description{
Judges and converts to hydrologic calendar from date.
Note that the hydrologic year starts 01 Oct in the
Northern hemisphere and 01 Aug in the Southern and
tropical regions.
}
