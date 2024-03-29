% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/e_star.R
\name{e_star}
\alias{e_star}
\title{Saturation vapor pressure as a function of temperature}
\usage{
e_star(temp, unit = "C")
}
\arguments{
\item{temp}{temperature}

\item{unit}{temperature unit (C)elcius, (K)elvin, (F)ahrenheit, (R)ankine}
}
\value{
saturation vapor pressure (Pa)
}
\description{
This function returns the saturation vapor
pressure of water in the atmosphere using the
nested form of the Goff-Gratch formulation as
derived by Lowe (1977).  Input temperature is
provided in Celcius, but can be adjusted to
F, K, or R.  The output is in Pascals.
}
