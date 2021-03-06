% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/season.R
\name{ps_add_season}
\alias{ps_add_season}
\title{Add Season}
\usage{
ps_add_season(
  x,
  date = "Date",
  season = "Season",
  year_season = "YearSeason",
  seasons = c(Spring = 3L, Summer = 6L, Autumn = 9L, Winter = 12L)
)
}
\arguments{
\item{x}{A data frame}

\item{date}{A string of the name of the column with the Dates or POSIXt objects.}

\item{season}{A string of the name of the column to save the seasons.}

\item{year_season}{A string of the name of the column to save the year of the season.}

\item{seasons}{A uniquely named integer vector of the first month of each season.}
}
\value{
An original data frame with a season and year column.
}
\description{
Returns a data frame with the season and the year.
If the first month of the first season isn't January (1L), then
the last season is considered to wrap into the following year.
}
\examples{
x <- data.frame(Date = as.Date(c("2000-12-31", "2001-01-01", "2001-06-01", "2001-12-31")))
ps_add_season(x)
}
