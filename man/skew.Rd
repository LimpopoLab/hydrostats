% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/skew.R
\name{skew}
\alias{skew}
\title{Skewness}
\usage{
skew(x)
}
\arguments{
\item{x}{data}
}
\value{
skewness
}
\description{
This function returns the skewness of the data.  This
function is writen to remove NAs and use the bias-
corrected statistics.  The third moment about the mean
is computed and divided by the cube of the standard
deviation.
}
