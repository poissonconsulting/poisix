# Get Day of the Year

Returns the day of the year as an integer between 1 and 366.

## Usage

``` r
doy(x)
```

## Arguments

- x:

  The object.

## Value

An integer vector with values between 1 and 366.

## Examples

``` r
doy(as.Date("2002-01-11"))
#> [1] 11
doy(as.Date("2001-05-16"))
#> [1] 136
```
