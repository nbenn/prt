
tmp <- tempfile()

setup(dir.create(tmp))
teardown(unlink(tmp, recursive = TRUE))

prt_cars <- create_prt(demo_data_frame("mtcars"), dir = tmp, n_chunks = 2L)
prt_iris <- create_prt(demo_data_frame("iris"), dir = tmp, n_chunks = 2L)

prt_all <- create_prt(df_all, dir = tmp)

test_that("interface of print() identical to trunc_dt()", {
  print_arg_names <- names(formals(print.prt))
  trunc_dt_arg_names <- names(formals(trunc_dt))

  expect_equal(setdiff(print_arg_names, "..."), trunc_dt_arg_names)
})

test_that("print() returns output invisibly", {
  expect_output(ret <- withVisible(print(prt_cars)))
  expect_false(ret$visible)
  expect_identical(ret$value, prt_cars)
})

test_that("trunc_dt output matches known output", {

  skip_on_non_utf8_locale()

  expect_output_file_opts(
    print(prt_cars, n = 8L, width = 30L),
    "format/mtcars-8-30.txt"
  )

  expect_output_file_opts(
    print(prt_iris, n = 5L, width = 30L),
    "format/iris-5-30.txt"
  )

  expect_output_file_opts(
    print(prt_iris, n = -1L, width = 30L),
    "format/iris-neg-30.txt"
  )

  expect_output_file_opts(
    print(prt_iris, n = Inf, width = 30L),
    "format/iris-inf-30.txt"
  )

  expect_output_file_opts(
    print(prt_iris, n = 3L, width = 5L),
    "format/iris-3-5.txt"
  )

  expect_output_file_opts(
    print(prt_iris, n = NULL, width = 70L),
    "format/iris--70.txt"
  )

  expect_output_file_opts(
    print(prt_all, n = NULL, width = 30L),
    "format/all--30.txt"
  )

  expect_output_file_opts(
    print(prt_all, n = NULL, width = 300L),
    "format/all--300.txt"
  )

  expect_output_file_opts(
    print(create_prt(tibble::tibble(a = seq.int(10000)), dir = tmp), n = 5L,
          width = 30L),
    "format/long-5-30.txt"
  )

  expect_output_file_opts(
    print(trunc_dt(prt_all, n = 1L, n_extra = 2L, width = 30L)),
    "format/all-1-30-2.txt"
  )

  expect_output_file_opts(
    print(trunc_dt(prt_all, n = 1L, n_extra = 0L, width = 30L)),
    "format/all-1-30-0.txt"
  )

  expect_output_file_opts(
    print(
      trunc_dt(
        create_prt(tibble::tibble("mean(x)" = 5, "var(x)" = 3), dir = tmp),
        width = 28
      )
    ),
    "format/non-syntactic.txt"
  )
})

test_that("trunc_mat for POSIXct columns", {

  skip_on_os("windows")
  skip_on_non_utf8_locale()

  df <- tibble::tibble(x = as.POSIXct("2016-01-01 12:34:56 GMT") + 1:12)

  expect_output_file_opts(
    print(create_prt(df, dir = tmp), n = 8L, width = 60L),
    "format/POSIXct-8-60.txt"
  )
})

test_that("trunc_mat for wide-character columns", {

  skip_on_os("windows")
  skip_on_non_utf8_locale()

  x <- c("\u6210\u4ea4\u65e5\u671f", "\u5408\u540c\u5f55\u5165\u65e5\u671f")
  df <- setNames(tibble::tibble(1:3, 4:6), x)

  expect_output_file_opts(
    print(create_prt(df, dir = tmp), n = 8L, width = 60L),
    "format/wide-8-60.txt"
  )
})
