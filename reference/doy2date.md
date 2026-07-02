# Day of the Year as an integer to a Date

Day of the Year as an integer to a Date

## Usage

``` r
doy2date(x, year)
```

## Arguments

- x:

  The integer vector

- year:

  An integer vector of the year(s)

## Value

A Date vector.

## Examples

``` r
doy2date(1:2, 2000)
#> [1] "2000-01-01" "2000-01-02"
doy2date(1:2, 2002:2001)
#> [1] "2002-01-01" "2001-01-02"
```
