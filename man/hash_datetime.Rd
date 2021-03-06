% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binning.R
\name{hash_datetime}
\alias{hash_datetime}
\title{Hash date time}
\usage{
hash_datetime(dates, resolution = "week")
}
\arguments{
\item{dates}{Date objects to be binned.}

\item{resolution}{Temporal resolution to be used. Either one of the following
string literals: \code{"day"}, \code{"week"}, \code{"month"}, \code{"year"}
or a date time format string e.g. \code{"\%Y-\%m-\%d"}.}
}
\value{
character vector with unique characters for each bin.
}
\description{
Hash dates according to the given temporal resolution.
}
