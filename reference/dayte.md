# Get Day of the year as a Date

Returns the day of the year as a Date by setting the year to be 1972 (as
a leap year with two leap seconds)

## Usage

``` r
dayte(x)
```

## Arguments

- x:

  The object.

## Value

A Date or POSIXt object.

## Examples

``` r
dayte(as.Date("2001-05-16"))
#> [1] "1972-05-16"
dayte(as.Date("2004-02-29"))
#> [1] "1972-02-29"
```
