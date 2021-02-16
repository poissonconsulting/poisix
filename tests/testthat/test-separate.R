test_that("ps_separate_date", {

  data <- data.frame(Date = as.Date("2001-02-03"), Other = 1)
  expect_identical(ps_separate_date(data), data.frame(
    Other = 1, Year = 2001L, Month = 2L, Day = 3L))

  expect_identical(ps_separate_date(data, remove = FALSE), data.frame(
    Date = as.Date("2001-02-03"), Other = 1, Year = 2001L, Month = 2L, Day = 3L))
})

test_that("ps_separate_datetime", {
  data <- data.frame(DateTime = as.POSIXct("2001-02-03 04:05:07"), Other = 1)
  expect_identical(ps_separate_datetime(data), data.frame(
    Other = 1, Year = 2001L, Month = 2L, Day = 3L, Hour = 4L, Minute = 5L, Second = 7L))
})

