# Add Season

Returns a data frame with the season and the year. If the first month of
the first season isn't January (1L), then the last season is considered
to wrap into the following year.

## Usage

``` r
ps_add_season(
  x,
  date = "Date",
  season = "Season",
  year_season = "YearSeason",
  seasons = c(Spring = 3L, Summer = 6L, Autumn = 9L, Winter = 12L)
)
```

## Arguments

- x:

  A data frame

- date:

  A string of the name of the column with the Dates or POSIXt objects.

- season:

  A string of the name of the column to save the seasons.

- year_season:

  A string of the name of the column to save the year of the season.

- seasons:

  A uniquely named integer vector of the first month of each season.

## Value

An original data frame with a season and year column.

## Examples

``` r
x <- data.frame(Date = as.Date(c("2000-12-31", "2001-01-01", "2001-06-01", "2001-12-31")))
ps_add_season(x)
#>         Date Season YearSeason
#> 1 2000-12-31 Winter       2000
#> 2 2001-01-01 Winter       2000
#> 3 2001-06-01 Summer       2001
#> 4 2001-12-31 Winter       2001
```
