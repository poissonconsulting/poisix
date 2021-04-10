test_that("doy", {
  expect_identical(doy(as.Date("2002-01-11")), 11L)
  expect_identical(doy(as.Date("2001-05-16")), 136L)
  expect_identical(doy(as.Date("2001-12-31")), 365L)
  expect_identical(doy(as.Date("2004-12-31")), 366L)

  expect_identical(doy(as.Date(c("2004-12-31", "2001-12-31", NA))), c(366L, 365L, NA))
})

test_that("doy2date", {
  expect_identical(doy2date(1:2, 2000), as.Date(c("2000-01-01", "2000-01-02")))
  expect_identical(doy2date(1:2, 2002:2001), as.Date(c("2002-01-01", "2001-01-02")))
  expect_error(doy2date(1:2, "2000"), "...", class = "chk_error")
  expect_error(doy2date(1:2, factor("2000")), "...", class = "chk_error")
})

test_that("dayte", {
  expect_identical(dayte(as.Date("2001-05-16")), as.Date("1972-05-16"))
  expect_identical(dayte(as.Date("2004-02-29")), as.Date("1972-02-29"))
  expect_identical(dayte(as.Date(c("2004-02-29", NA, "2001-05-16"))), as.Date(c("1972-02-29", NA, "1972-05-16")))
})

test_that("dayte_time", {
  expect_equal(dayte_time(as.POSIXct("2001-05-16 02:03:04")),
                   as.POSIXct("1972-05-16 02:03:04"))
})
