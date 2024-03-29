#' Separate Date
#'
#' Separates Date into Year, Month and Day.
#' @param data A data frame.
#' @param col A column name or position.
#' @param into A character vector of length 3 specifying the names for the year, month and day components.
#' @param remove A flag specifying whether to remove the original column.
#' @export
#'
#' @examples
#' data <- data.frame(Date = Sys.Date())
#' ps_separate_date(data)
ps_separate_date <- function(data, col = "Date",
                             into = c("Year", "Month", "Day"),
                             remove = TRUE) {
  if(!is.data.frame(data))
    error("data must be a data frame")

  col %<>% tidyselect::vars_pull(colnames(data), .)

  chk_string(col)
  chk_character(into)
  check_dim(into, values = 3)
  chk_unique(into)
  chk_flag(remove)

  data[[into[1]]] <- dttr2::dtt_year(data[[col]]) %>% as.integer()
  data[[into[2]]] <- dttr2::dtt_month(data[[col]]) %>% as.integer()
  data[[into[3]]] <- dttr2::dtt_day(data[[col]]) %>% as.integer()

  if(remove) data[[col]] <- NULL

  data
}

#' Separate DateTime
#'
#' Separates DateTime into Year, Month and Day.
#' @param data A data frame.
#' @param col A column name or position.
#' @param into A character vector of length 6 specifying the names for the year, month, day, hour, minute and second components.
#' @param remove A flag specifying whether to remove the original column.
#' @export
#'
#' @examples
#' data <- data.frame(DateTime = Sys.time())
#' ps_separate_datetime(data)
ps_separate_datetime <- function(data, col = "DateTime",
                                 into = c("Year", "Month", "Day", "Hour", "Minute", "Second"),
                                 remove = TRUE) {
  if(!is.data.frame(data))
    error("data must be a data frame")

  col %<>% tidyselect::vars_pull(colnames(data), .)

  chk_string(col)
  check_values(into, "")
  chk_unique(into)
  check_dim(into, values = 6)
  chk_flag(remove)

  data[[into[1]]] <- dttr2::dtt_year(data[[col]]) %>% as.integer()
  data[[into[2]]] <- dttr2::dtt_month(data[[col]]) %>% as.integer()
  data[[into[3]]] <- dttr2::dtt_day(data[[col]]) %>% as.integer()
  data[[into[4]]] <- dttr2::dtt_hour(data[[col]]) %>% as.integer()
  data[[into[5]]] <- dttr2::dtt_minute(data[[col]]) %>% as.integer()
  data[[into[6]]] <- dttr2::dtt_second(data[[col]]) %>% as.integer()

  if(remove) data[[col]] <- NULL

  data
}
