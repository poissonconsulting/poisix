# Separate Date

Separates Date into Year, Month and Day.

## Usage

``` r
ps_separate_date(
  data,
  col = "Date",
  into = c("Year", "Month", "Day"),
  remove = TRUE
)
```

## Arguments

- data:

  A data frame.

- col:

  A column name or position.

- into:

  A character vector of length 3 specifying the names for the year,
  month and day components.

- remove:

  A flag specifying whether to remove the original column.

## Examples

``` r
data <- data.frame(Date = Sys.Date())
ps_separate_date(data)
#>   Year Month Day
#> 1 2026     7   2
```
