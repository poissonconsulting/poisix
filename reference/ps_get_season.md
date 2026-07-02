# Get Season

Returns an ordered factor the user specified seasons. If the first month
of the first season isn't January (1L), then the last season is
considered to wrap into the following year.

## Usage

``` r
ps_get_season(
  x,
  seasons = c(Spring = 3L, Summer = 6L, Autumn = 9L, Winter = 12L)
)
```

## Arguments

- x:

  A Date or POSIXt vector

- seasons:

  A uniquely named integer vector of the first month of each season.

## Value

An ordered factor of the seasons.

## Examples

``` r
ps_get_season(as.Date(c("2001-01-01", "2001-02-28", "2012-09-01", "2012-12-01")))
#> [1] Winter Winter Autumn Winter
#> Levels: Winter < Spring < Summer < Autumn
ps_get_season(as.Date(c("2001-01-01", "2001-02-28", "2012-09-01", "2012-12-01")),
season = c(Monsoon = 2L, `Dry Period` = 6L))
#> [1] Dry Period Monsoon    Dry Period Dry Period
#> Levels: Dry Period < Monsoon
```
