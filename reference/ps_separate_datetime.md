# Separate DateTime

Separates DateTime into Year, Month and Day.

## Usage

``` r
ps_separate_datetime(
  data,
  col = "DateTime",
  into = c("Year", "Month", "Day", "Hour", "Minute", "Second"),
  remove = TRUE
)
```

## Arguments

- data:

  A data frame.

- col:

  A column name or position.

- into:

  A character vector of length 6 specifying the names for the year,
  month, day, hour, minute and second components.

- remove:

  A flag specifying whether to remove the original column.

## Examples

``` r
data <- data.frame(DateTime = Sys.time())
ps_separate_datetime(data)
#>   Year Month Day Hour Minute Second
#> 1 2026     7   2    0     14     52
```
