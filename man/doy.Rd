% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dayte.R
\name{doy}
\alias{doy}
\title{Get Day of the Year}
\usage{
doy(x)
}
\arguments{
\item{x}{The object.}
}
\value{
An integer vector with values between 1 and 366.
}
\description{
Returns the day of the year as an integer between 1 and 366.
}
\examples{
doy(as.Date("2002-01-11"))
doy(as.Date("2001-05-16"))
}
