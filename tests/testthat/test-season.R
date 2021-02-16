test_that("ps_get_season", {

  dates <- as.Date(c(
    "2001-01-01", "2011-05-30", "2001-02-28", "2011-03-01", "2013-06-01",
    "2012-09-01", "2012-12-31", NA))

  expect_identical(ps_get_season(dates),
    ordered(c("Winter", "Spring", "Winter", "Spring", "Summer", "Autumn", "Winter", NA),
    levels = c("Winter", "Spring", "Summer", "Autumn")))

  expect_identical(ps_get_season(dates,
    seasons = c(Spring = 1L, Summer = 6L, Autumn = 8L, Winter = 11L)),
    ordered(c("Spring", "Spring", "Spring", "Spring", "Summer", "Autumn", "Winter", NA),
    levels = c("Spring", "Summer", "Autumn", "Winter")))

  expect_identical(ps_get_season(dates,
    seasons = c(Monsoon = 2L, `Dry Period` = 6L)),
    ordered(c("Dry Period", "Monsoon", "Monsoon", "Monsoon", "Dry Period", "Dry Period", "Dry Period", NA),
    levels = c("Dry Period", "Monsoon")))

  expect_length(ps_get_season(dates[1][-1]), 0)
  expect_is(ps_get_season(dates[1][-1]), "ordered")
})

test_that("ps_add_season", {
  x <- data.frame(Date = as.Date(c("2000-12-31", "2001-01-01", "2001-06-01", "2001-12-31")))
  y <-  ps_add_season(x)
  expect_identical(colnames(y), c("Date", "Season", "YearSeason"))
  expect_identical(y$Season,
                   ordered(c("Winter", "Winter", "Summer", "Winter"),
                           levels = c("Winter", "Spring", "Summer", "Autumn")))
  expect_identical(y$Year, c(2000L, 2000L, 2001L, 2001L))

  z <- ps_add_season(x, year_season = "Yr", seasons = c(Spring = 1L, Summer = 6L, Winter = 12L))
  expect_identical(colnames(z), c("Date", "Season", "Yr"))
  expect_identical(z$Season,
                   ordered(c("Winter", "Spring", "Summer", "Winter"),
                           levels = c("Spring", "Summer", "Winter")))
  expect_identical(z$Yr, c(2000L, 2001L, 2001L, 2001L))
})
