# Get Day and time of the year as a POSIX

Returns the day of the year as a Date by setting the year to be 1972 (as
a leap year with two leap seconds)

## Usage

``` r
dayte_time(x)
```

## Arguments

- x:

  A POSIXt object.

## Value

A Date object.

## Examples

``` r
dayte_time(as.POSIXct("2001-05-16 02:03:04"))
#> [1] "1972-05-16 02:03:04 UTC"
```
