#' Get Season
#'
#' Returns an ordered factor the user specified seasons.
#' If the first month of the first season isn't January (1L), then
#' the last season is considered to wrap into the following year.
#'
#' @param x A Date or POSIXt vector
#' @param seasons A uniquely named integer vector of the first month of each season.
#'
#' @return An ordered factor of the seasons.
#' @export
#'
#' @examples
#' ps_get_season(as.Date(c("2001-01-01", "2001-02-28", "2012-09-01", "2012-12-01")))
#' ps_get_season(as.Date(c("2001-01-01", "2001-02-28", "2012-09-01", "2012-12-01")),
#' season = c(Monsoon = 2L, `Dry Period` = 6L))
ps_get_season <- function (x, seasons = c(Spring = 3L, Summer = 6L,
                                            Autumn = 9L, Winter = 12L)) {
  checkor(check_vector(x, c(Sys.Date(), NA)),
          check_vector(x, c(Sys.time(), NA)))
  check_vector(seasons, c(1L, 12L), length = c(1, .Machine$integer.max),
               unique = TRUE, sorted = TRUE)
  check_names(seasons, unique = TRUE)

  is_length <- length(x)
  if(!is_length) x <- as.Date("2000-01-01")

  if(seasons[1] != 1L) { # last season wraps
    start <- list(1L) %>% setNames(names(seasons[length(seasons)]))
    seasons %<>% c(start, .)
  }

  breaks <- paste("1972", seasons, "01", sep = "-") %>%
    c("1972-12-31") %>%
    as.Date() %>%
    dayte()

  breaks[length(breaks)] %<>% magrittr::add(lubridate::days(1L))

  x %<>%
    lubridate::date() %>%
    dayte() %>%
    cut(breaks = breaks, ordered_result = TRUE)

  levels(x) <- names(seasons)
  if(!is_length) x <- x[-1]
  x
}

#' Add Season
#'
#' Returns a data frame with the season and the year.
#' If the first month of the first season isn't January (1L), then
#' the last season is considered to wrap into the following year.
#'
#' @inheritParams ps_get_season
#' @param x A data frame
#' @param date A string of the name of the column with the Dates or POSIXt objects.
#' @param season A string of the name of the column to save the seasons.
#' @param year_season A string of the name of the column to save the year of the season.
#' @return An original data frame with a season and year column.
#' @export
#'
#' @examples
#' x <- data.frame(Date = as.Date(c("2000-12-31", "2001-01-01", "2001-06-01", "2001-12-31")))
#' ps_add_season(x)
ps_add_season <- function (x, date = "Date", season = "Season", year_season = "YearSeason",
                           seasons = c(Spring = 3L, Summer = 6L,
                                            Autumn = 9L, Winter = 12L)) {
  check_string(date)
  check_string(season)
  check_string(year_season)
  check_colnames(x, date)
  if(length(unique(c(date, season, year_season))) != 3)
    error("date, season and year_season must be unique")

  x[[season]] <- ps_get_season(x[[date]], seasons = seasons)
  x[[year_season]] <- lubridate::year(x[[date]]) %>% as.integer()

  if(seasons[[1]] != 1L) { # last season wraps
    x[[year_season]][lubridate::month(x[[date]]) < seasons[[1]]] %<>%
      magrittr::subtract(1L)
  }
  x
}
