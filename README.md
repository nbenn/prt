
<!-- README.md is generated from README.Rmd. Please edit that file -->

# prt

<!-- badges: start -->

[![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Codecov test
coverage](https://app.codecov.io/gh/nbenn/prt/branch/master/graph/badge.svg?token=HvOM3yosW3)](https://app.codecov.io/gh/nbenn/prt)
[![R build
status](https://github.com/nbenn/prt/workflows/build/badge.svg)](https://github.com/nbenn/prt/actions?query=workflow%3Abuild)
[![pkgdown build
status](https://github.com/nbenn/prt/workflows/pkgdown/badge.svg)](https://github.com/nbenn/prt/actions?query=workflow%3Apkgdown)
[![covr
status](https://github.com/nbenn/prt/workflows/coverage/badge.svg)](https://github.com/nbenn/prt/actions?query=workflow%3Acoverage)
<!-- badges: end -->

Building on `data.frame` serialization provided by
[`fst`](https://www.fstpackage.org), `prt` offers an interface for
working with partitioned `data.frame`s, saved as individual `fst` files.

## Installation

You can install the development version of
[prt](https://nbenn.github.io/prt/) from GitHub by running

``` r
source("https://install-github.me/nbenn/prt")
```

Alternatively, if you have the `remotes` package available, the latest
release is available by calling `install_github()` as

``` r
# install.packages("remotes")
remotes::install_github("nbenn/prt@*release")
```

## Short demo

Creating a `prt` object can be done either by calling `new_prt()` on a
list of previously created `fst` files or by coercing a `data.frame`
object to `prt` using `as_prt()`.

``` r
tmp <- tempfile()
dir.create(tmp)

flights <- as_prt(nycflights13::flights, n_chunks = 2L, dir = tmp)
#> fstcore package v0.9.14
#> (OpenMP was not detected, using single threaded mode)

print(flights)
#> # A prt:        336,776 × 19
#> # Partitioning: [168,388, 168,388] rows
#>          year month   day dep_time sched_dep_t…¹ dep_delay arr_time sched_arr_…²
#>         <int> <int> <int>    <int>         <int>     <dbl>    <int>        <int>
#> 1        2013     1     1      517           515         2      830          819
#> 2        2013     1     1      533           529         4      850          830
#> 3        2013     1     1      542           540         2      923          850
#> 4        2013     1     1      544           545        -1     1004         1022
#> 5        2013     1     1      554           600        -6      812          837
#> …
#> 336,772  2013     9    30       NA          1455        NA       NA         1634
#> 336,773  2013     9    30       NA          2200        NA       NA         2312
#> 336,774  2013     9    30       NA          1210        NA       NA         1330
#> 336,775  2013     9    30       NA          1159        NA       NA         1344
#> 336,776  2013     9    30       NA           840        NA       NA         1020
#> # ℹ 336,771 more rows
#> # ℹ abbreviated names: ¹​sched_dep_time, ²​sched_arr_time
#> # ℹ 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
#> #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
#> #   hour <dbl>, minute <dbl>, time_hour <dttm>
```

In case a `prt` object is created from a `data.frame`, the specified
number of files is written to the directory of choice (a newly created
directory within `tempdir()` by default).

``` r
list.files(tmp)
#> [1] "1.fst" "2.fst"
```

Subsetting and printing is closely modeled after `tibble` and behavior
that deviates from that of `tibble` will most likely be considered a bug
(please [report](https://github.com/nbenn/prt/issues/new)). Some design
choices that do set a `prt` object apart from a `tibble` include the use
of `data.table`s for any result of a subsetting operation and the
complete disregard for `row.names`.

In addition to standard subsetting operations involving the functions
<code>\`\[\`()</code>, <code>\`\[\[\`()</code> and
<code>\`\$\`()</code>, the base generic function `subset()` is
implemented for the `prt` class, enabling subsetting operations using
non-standard evaluation. Combined with random access to tables stored as
`fst` files, this can make data access more efficient in cases where
only a subset of the data is of interest.

``` r
jan <- flights[flights$month == 1, ]
identical(jan, subset(flights, month == 1))
#> [1] TRUE
print(jan)
#>        year month day dep_time sched_dep_time dep_delay arr_time sched_arr_time
#>     1: 2013     1   1      517            515         2      830            819
#>     2: 2013     1   1      533            529         4      850            830
#>     3: 2013     1   1      542            540         2      923            850
#>     4: 2013     1   1      544            545        -1     1004           1022
#>     5: 2013     1   1      554            600        -6      812            837
#>    ---                                                                         
#> 27000: 2013     1  31       NA           1325        NA       NA           1505
#> 27001: 2013     1  31       NA           1200        NA       NA           1430
#> 27002: 2013     1  31       NA           1410        NA       NA           1555
#> 27003: 2013     1  31       NA           1446        NA       NA           1757
#> 27004: 2013     1  31       NA            625        NA       NA            934
#>        arr_delay carrier flight tailnum origin dest air_time distance hour
#>     1:        11      UA   1545  N14228    EWR  IAH      227     1400    5
#>     2:        20      UA   1714  N24211    LGA  IAH      227     1416    5
#>     3:        33      AA   1141  N619AA    JFK  MIA      160     1089    5
#>     4:       -18      B6    725  N804JB    JFK  BQN      183     1576    5
#>     5:       -25      DL    461  N668DN    LGA  ATL      116      762    6
#>    ---                                                                    
#> 27000:        NA      MQ   4475  N730MQ    LGA  RDU       NA      431   13
#> 27001:        NA      MQ   4658  N505MQ    LGA  ATL       NA      762   12
#> 27002:        NA      MQ   4491  N734MQ    LGA  CLE       NA      419   14
#> 27003:        NA      UA    337    <NA>    LGA  IAH       NA     1416   14
#> 27004:        NA      UA   1497    <NA>    LGA  IAH       NA     1416    6
#>        minute           time_hour
#>     1:     15 2013-01-01 05:00:00
#>     2:     29 2013-01-01 05:00:00
#>     3:     40 2013-01-01 05:00:00
#>     4:     45 2013-01-01 05:00:00
#>     5:      0 2013-01-01 06:00:00
#>    ---                           
#> 27000:     25 2013-01-31 13:00:00
#> 27001:      0 2013-01-31 12:00:00
#> 27002:     10 2013-01-31 14:00:00
#> 27003:     46 2013-01-31 14:00:00
#> 27004:     25 2013-01-31 06:00:00
```

A subsetting operation on a `prt` object yields a `data.table`. If the
full table is of interest, a `prt`-specific implementation of the
`as.data.table()` generic is available.

``` r
unlink(tmp, recursive = TRUE)
```
