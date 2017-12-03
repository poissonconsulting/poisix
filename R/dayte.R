#' Get Day of the Year
#'
#' Returns the day of the year as an integer between 1 and 366.
#'
#' @param x The object.
#' @return An integer vector between 1 and 366.
#' @export
#' @examples
#' doy(as.Date("2002-01-11"))
#' doy(as.Date("2001-05-16"))
doy <- function(x){
  checkor(check_vector(x, c(Sys.Date(), NA)), check_vector(x, c(Sys.time(), NA)))
  x %<>% date()
  y <- as.Date(paste(year(x) - 1, 12, 31, sep = "-"))
  x %<>% difftime(y, units = "days") %>% as.integer()
  x
}

#' Get Day of the year as a Date
#'
#' Returns the day of the year as a Date by setting the
#' year to be 1972 (as a leap year with two leap seconds)
#'
#' @param x The object.
#' @return A Date or POSIXt object.
#' @export
#' @examples
#' dayte(as.Date("2001-05-16"))
#' dayte(as.Date("2004-02-29"))
dayte <- function(x){
  checkor(check_vector(x, c(Sys.Date(), NA)), check_vector(x, c(Sys.time(), NA)))
  x %<>% date()
  year(x) <- 1972
  x
}

#' Get Day and time of the year as a POSIX
#'
#' Returns the day of the year as a Date by setting the
#' year to be 1972 (as a leap year with two leap seconds)
#'
#' @param x A POSIXt object.
#' @return A Date object.
#' @export
#' @examples
#' dayte_time(as.POSIXct("2001-05-16 02:03:04"))
dayte_time <- function(x){
  check_vector(x, c(Sys.time(), NA))
  year(x) <- 1972
  x
}
