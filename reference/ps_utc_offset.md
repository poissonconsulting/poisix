# UTC Offset

Calculates the offsets in hours between the time zones of a POSIXct
object and the coordinated Universal Time time zone.

## Usage

``` r
ps_utc_offset(x)
```

## Arguments

- x:

  A POSIXct object.

## Value

The offsets as a numeric vector.

## Examples

``` r
times <- c("2001-06-01 00:00:00", "2001-01-01 00:00:00")
times <- as.POSIXct(times, tz = "PST8PDT")
times
#> [1] "2001-06-01 PDT" "2001-01-01 PST"
ps_utc_offset(times)
#> [1] -7 -8
```
