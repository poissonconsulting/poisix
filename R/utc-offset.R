#' UTC Offset
#'
#' Calculates the offsets in hours between the time zones
#' of a POSIXct object and the coordinated Universal Time time zone.
#'
#' @param x A POSIXct object.
#' @return The offsets as a numeric vector.
#' @export
#' @examples
#' times <- c("2001-06-01 00:00:00", "2001-01-01 00:00:00")
#' times <- as.POSIXct(times, tz = "PST8PDT")
#' times
#' ps_utc_offset(times)
ps_utc_offset <- function(x) {
  check_values(x, c(Sys.time(), NA))
  check_dim(x, values = TRUE)
  x %<>%
    difftime(force_tz(., "UTC"), ., units = "hours") %>%
    as.numeric()
  x
}
