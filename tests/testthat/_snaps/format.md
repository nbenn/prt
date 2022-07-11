# trunc_dt output matches known output

    Code
      print(prt_cars, n = 8L, width = 30L)
    Output
      # A prt:        32 × 11
      # Partitioning: [16, 16] rows
           mpg   cyl  disp    hp
         <dbl> <dbl> <dbl> <dbl>
       1  21       6 160     110
       2  21       6 160     110
       3  22.8     4 108      93
       4  21.4     6 258     110
       5  18.7     8 360     175
       6  18.1     6 225     105
       7  14.3     8 360     245
       8  24.4     4 147.     62
       …
      25  19.2     8 400     175
      26  27.3     4  79      66
      27  26       4 120.     91
      28  30.4     4  95.1   113
      29  15.8     8 351     264
      30  19.7     6 145     175
      31  15       8 301     335
      32  21.4     4 121     109
      # … with 16 more rows, and 7
      #   more variables:
      #   drat <dbl>, wt <dbl>,
      #   qsec <dbl>, vs <dbl>,
      #   am <dbl>, gear <dbl>,
      #   carb <dbl>
    Code
      print(prt_iris, n = 5L, width = 30L)
    Output
      # A prt:        150 × 5
      # Partitioning: [75, 75] rows
          Sepal.Length Sepal.Width
                 <dbl>       <dbl>
        1          5.1         3.5
        2          4.9         3
        3          4.7         3.2
        4          4.6         3.1
        5          5           3.6
        …
      146          6.7         3
      147          6.3         2.5
      148          6.5         3
      149          6.2         3.4
      150          5.9         3
      # … with 140 more rows, and 3
      #   more variables:
      #   Petal.Length <dbl>,
      #   Petal.Width <dbl>,
      #   Species <fct>
    Code
      print(prt_iris, n = -1L, width = 30L)
    Output
      # A prt:        150 × 5
      # Partitioning: [75, 75] rows
          Sepal.Length Sepal.Width
                 <dbl>       <dbl>
        1          5.1         3.5
        2          4.9         3
        3          4.7         3.2
        4          4.6         3.1
        5          5           3.6
        …
      146          6.7         3
      147          6.3         2.5
      148          6.5         3
      149          6.2         3.4
      150          5.9         3
      # … with 140 more rows, and 3
      #   more variables:
      #   Petal.Length <dbl>,
      #   Petal.Width <dbl>,
      #   Species <fct>
    Code
      print(prt_iris, n = Inf, width = 30L)
    Output
      # A prt:        150 × 5
      # Partitioning: [75, 75] rows
          Sepal.Length Sepal.Width
                 <dbl>       <dbl>
        1          5.1         3.5
        2          4.9         3
        3          4.7         3.2
        4          4.6         3.1
        5          5           3.6
        6          5.4         3.9
        7          4.6         3.4
        8          5           3.4
        9          4.4         2.9
       10          4.9         3.1
       11          5.4         3.7
       12          4.8         3.4
       13          4.8         3
       14          4.3         3
       15          5.8         4
       16          5.7         4.4
       17          5.4         3.9
       18          5.1         3.5
       19          5.7         3.8
       20          5.1         3.8
       21          5.4         3.4
       22          5.1         3.7
       23          4.6         3.6
       24          5.1         3.3
       25          4.8         3.4
       26          5           3
       27          5           3.4
       28          5.2         3.5
       29          5.2         3.4
       30          4.7         3.2
       31          4.8         3.1
       32          5.4         3.4
       33          5.2         4.1
       34          5.5         4.2
       35          4.9         3.1
       36          5           3.2
       37          5.5         3.5
       38          4.9         3.6
       39          4.4         3
       40          5.1         3.4
       41          5           3.5
       42          4.5         2.3
       43          4.4         3.2
       44          5           3.5
       45          5.1         3.8
       46          4.8         3
       47          5.1         3.8
       48          4.6         3.2
       49          5.3         3.7
       50          5           3.3
       51          7           3.2
       52          6.4         3.2
       53          6.9         3.1
       54          5.5         2.3
       55          6.5         2.8
       56          5.7         2.8
       57          6.3         3.3
       58          4.9         2.4
       59          6.6         2.9
       60          5.2         2.7
       61          5           2
       62          5.9         3
       63          6           2.2
       64          6.1         2.9
       65          5.6         2.9
       66          6.7         3.1
       67          5.6         3
       68          5.8         2.7
       69          6.2         2.2
       70          5.6         2.5
       71          5.9         3.2
       72          6.1         2.8
       73          6.3         2.5
       74          6.1         2.8
       75          6.4         2.9
       76          6.6         3
       77          6.8         2.8
       78          6.7         3
       79          6           2.9
       80          5.7         2.6
       81          5.5         2.4
       82          5.5         2.4
       83          5.8         2.7
       84          6           2.7
       85          5.4         3
       86          6           3.4
       87          6.7         3.1
       88          6.3         2.3
       89          5.6         3
       90          5.5         2.5
       91          5.5         2.6
       92          6.1         3
       93          5.8         2.6
       94          5           2.3
       95          5.6         2.7
       96          5.7         3
       97          5.7         2.9
       98          6.2         2.9
       99          5.1         2.5
      100          5.7         2.8
      101          6.3         3.3
      102          5.8         2.7
      103          7.1         3
      104          6.3         2.9
      105          6.5         3
      106          7.6         3
      107          4.9         2.5
      108          7.3         2.9
      109          6.7         2.5
      110          7.2         3.6
      111          6.5         3.2
      112          6.4         2.7
      113          6.8         3
      114          5.7         2.5
      115          5.8         2.8
      116          6.4         3.2
      117          6.5         3
      118          7.7         3.8
      119          7.7         2.6
      120          6           2.2
      121          6.9         3.2
      122          5.6         2.8
      123          7.7         2.8
      124          6.3         2.7
      125          6.7         3.3
      126          7.2         3.2
      127          6.2         2.8
      128          6.1         3
      129          6.4         2.8
      130          7.2         3
      131          7.4         2.8
      132          7.9         3.8
      133          6.4         2.8
      134          6.3         2.8
      135          6.1         2.6
      136          7.7         3
      137          6.3         3.4
      138          6.4         3.1
      139          6           3
      140          6.9         3.1
      141          6.7         3.1
      142          6.9         3.1
      143          5.8         2.7
      144          6.8         3.2
      145          6.7         3.3
      146          6.7         3
      147          6.3         2.5
      148          6.5         3
      149          6.2         3.4
      150          5.9         3
      # … with 3 more variables:
      #   Petal.Length <dbl>,
      #   Petal.Width <dbl>,
      #   Species <fct>
    Code
      print(prt_iris, n = 3L, width = 5L)
    Output
      # A
      #   prt:
      #   150
      #   ×
      #   5
      # Partitioning:
      #   [75,
      #   75]
      #   rows
      # …
      #   with
      #   144
      #   more
      #   rows,
      #   and
      #   5
      #   more
      #   variables:
      #   Sepal.Length <dbl>,
      #   Sepal.Width <dbl>,
      #   Petal.Length <dbl>,
      #   Petal.Width <dbl>,
      #   Species <fct>
    Code
      print(prt_iris, n = NULL, width = 70L)
    Output
      # A prt:        150 × 5
      # Partitioning: [75, 75] rows
          Sepal.Length Sepal.Width Petal.Length Petal.Width Species
                 <dbl>       <dbl>        <dbl>       <dbl> <fct>
        1          5.1         3.5          1.4         0.2 setosa
        2          4.9         3            1.4         0.2 setosa
        3          4.7         3.2          1.3         0.2 setosa
        4          4.6         3.1          1.5         0.2 setosa
        5          5           3.6          1.4         0.2 setosa
        …
      146          6.7         3            5.2         2.3 virginica
      147          6.3         2.5          5           1.9 virginica
      148          6.5         3            5.2         2   virginica
      149          6.2         3.4          5.4         2.3 virginica
      150          5.9         3            5.1         1.8 virginica
      # … with 140 more rows
    Code
      print(prt_all, n = NULL, width = 30L)
    Output
      # A prt:        3 × 7
      # Partitioning: [3] rows
            a     b c     d
        <dbl> <int> <lgl> <chr>
      1   1       1 TRUE  a
      2   2.5     2 FALSE b
      3  NA      NA NA    <NA>
      # … with 3 more variables:
      #   e <fct>, f <date>,
      #   g <dttm>
    Code
      print(prt_all, n = NULL, width = 300L)
    Output
      # A prt:        3 × 7
      # Partitioning: [3] rows
            a     b c     d     e     f          g
        <dbl> <int> <lgl> <chr> <fct> <date>     <dttm>
      1   1       1 TRUE  a     a     2015-12-10 2015-12-09 10:51:35
      2   2.5     2 FALSE b     b     2015-12-11 2015-12-09 10:51:36
      3  NA      NA NA    <NA>  <NA>  NA         NA
    Code
      print(create_prt(tibble::tibble(a = seq.int(10000)), dir = tmp), n = 5L, width = 30L)
    Output
      # A prt:        10,000 × 1
      # Partitioning: [10,000] rows
                 a
             <int>
           1     1
           2     2
           3     3
           4     4
           5     5
           …
       9,996  9996
       9,997  9997
       9,998  9998
       9,999  9999
      10,000 10000
      # … with 9,990 more rows
    Code
      print(trunc_dt(prt_all, n = 1L, max_extra_cols = 2L, width = 30L))
    Output
      # A prt:        3 × 7
      # Partitioning: [3] rows
            a     b c     d
        <dbl> <int> <lgl> <chr>
      1     1     1 TRUE  a
      …
      3    NA    NA NA    <NA>
      # … with 1 more row, and 3
      #   more variables: e <fct>,
      #   f <date>, …
    Code
      print(trunc_dt(prt_all, n = 1L, max_extra_cols = 0L, width = 30L))
    Output
      # A prt:        3 × 7
      # Partitioning: [3] rows
            a     b c     d
        <dbl> <int> <lgl> <chr>
      1     1     1 TRUE  a
      …
      3    NA    NA NA    <NA>
      # … with 1 more row, and 3
      #   more variables
    Code
      print(trunc_dt(create_prt(tibble::tibble(`mean(x)` = 5, `var(x)` = 3), dir = tmp),
      width = 28))
    Output
      # A prt:        1 × 2
      # Partitioning: [1] rows
        `mean(x)` `var(x)`
            <dbl>    <dbl>
      1         5        3

