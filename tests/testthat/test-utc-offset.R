context("utc-offset")

test_that("utc-offset", {
  times <- c("2001-06-01 00:00:00", "2001-01-01 00:00:00")
  times <- as.POSIXct(times, tz = "PST8PDT")

  expect_identical(ps_utc_offset(times), c(-7, -8))
})
