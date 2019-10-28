
<!-- README.md is generated from README.Rmd. Please edit that file -->

# prt

<!-- badges: start -->

[![Travis pkgdown build
status](https://travis-ci.org/nbenn/prt.svg?branch=master)](https://travis-ci.org/nbenn/prt)
[![Azure pipelines build
status](https://img.shields.io/azure-devops/build/nbenn/prt/1)](https://dev.azure.com/nbenn/prt/_build/latest?definitionId=1&branchName=master)
[![Azure pipelines test
status](https://img.shields.io/azure-devops/tests/nbenn/prt/1?color=brightgreen&compact_message)](https://dev.azure.com/nbenn/prt/_build/latest?definitionId=1&branchName=master)
[![Azure pipelines coverage
status](https://img.shields.io/azure-devops/coverage/nbenn/prt/1)](https://dev.azure.com/nbenn/prt/_build/latest?definitionId=1&branchName=master)
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
flights <- as_prt(nycflights13::flights, n_chunks = 2L, dir = tmp)
print(flights)
#> # A prt:        336,776 × 19
#> # Partitioning: 2 parts [168388, 168388] rows
#>          year month   day dep_time sched_dep_time dep_delay arr_time
#>         <int> <int> <int>    <int>          <int>     <dbl>    <int>
#> 1        2013     1     1      517            515         2      830
#> 2        2013     1     1      533            529         4      850
#> 3        2013     1     1      542            540         2      923
#> 4        2013     1     1      544            545        -1     1004
#> 5        2013     1     1      554            600        -6      812
#> …
#> 336,772  2013     9    30       NA           1455        NA       NA
#> 336,773  2013     9    30       NA           2200        NA       NA
#> 336,774  2013     9    30       NA           1210        NA       NA
#> 336,775  2013     9    30       NA           1159        NA       NA
#> 336,776  2013     9    30       NA            840        NA       NA
#> # … with 336,766 more rows, and 12 more variables: sched_arr_time <int>,
#> #   arr_delay <dbl>, carrier <chr>, flight <int>, tailnum <chr>,
#> #   origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>,
#> #   minute <dbl>, time_hour <dttm>
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
