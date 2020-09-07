
test_that("format_row for values", {
  expect_equal(format_row(1), "1")
  expect_equal(format_row(1:3), c("1", "2", "3"))
  expect_equal(format_row(NA), "NA")
  expect_equal(format_row(TRUE), "TRUE")
  expect_equal(format_row(logical()), character())
  expect_equal(format_row(matrix(1:6, ncol = 2)), "<matrix[3 x 2]>")
})

test_that("format_row for character", {
  expect_equal(format_row("1"), paste0('"', "1", '"'))
  expect_equal(format_row(letters), paste0('"', letters, '"'))
  expect_equal(format_row(NA_character_), "NA")
  expect_equal(format_row(character()), character())
})

test_that("format_row for factor", {
  expect_equal(format_row(factor(c("1", "a"))), c("1", "a"))
  expect_equal(format_row(factor(c("foo", '"bar"'))), c("foo", "\"bar\""))
  expect_equal(format_row(factor()), character())
  expect_equal(
    format_row(factor(c("foo, bar", "foo", '"bar"'))),
    paste0('"', c("foo, bar", "foo", "\\\"bar\\\""), '"')
  )
})

tmp <- tempfile()

setup(dir.create(tmp))
teardown(unlink(tmp, recursive = TRUE))

prt_cars <- create_prt(demo_data_frame("mtcars"), dir = tmp, n_chunks = 2L)
prt_iris <- create_prt(demo_data_frame("iris"), dir = tmp, n_chunks = 2L)

prt_all <- create_prt(df_all, dir = tmp)

test_that("str_sum() works", {

  expect_match(
    str_sum(prt_cars), "'prt':\\s+32 obs\\. of 11 variables in 2 partitions"
  )

  expect_match(
    str_sum(mtcars), "'data.frame':\\s+32 obs. of 11 variables"
  )
})

test_that("glimpse output matches known output", {

  skip_on_non_utf8_locale()

  expect_output_file_opts(
    glimpse(prt_cars, width = 70L),
    "glimpse/mtcars-70.txt"
  )

  expect_output_file_opts(
    glimpse(prt_iris, width = 70L),
    "glimpse/iris-70.txt"
  )

  expect_output_file_opts(
    glimpse(
      create_prt(tibble::tibble("mean(x)" = 5, "var(x)" = 3), dir = tmp),
      width = 28
    ),
    "glimpse/non-syntactic.txt"
  )

  expect_output_file_opts(
    glimpse(prt_all, width = 70L),
    "glimpse/all-70.txt"
  )

  expect_output_file_opts(
    glimpse(prt_all),
    "glimpse/all-50.txt",
    tibble.width = 50
  )

  expect_output_file_opts(
    glimpse(prt_all),
    "glimpse/all-35.txt",
    tibble.width = 35
  )
})

test_that("glimpse(width = Inf) raises legible error", {
  expect_error(
    glimpse(prt_cars, width = Inf),
    "`glimpse()` requires a finite value for the `width` argument.",
    fixed = TRUE
  )
})

test_that("str output matches known output", {

  expect_output(res <- str(prt_cars))
  expect_null(res)

  expect_output_file_opts(
    str(prt_cars),
    "str/mtcars.txt"
  )

  expect_output_file_opts(
    str(prt_cars, vec.len = 10),
    "str/mtcars-10.txt"
  )

  expect_output_file_opts(
    str(prt_cars, no.list = TRUE),
    "str/mtcars.txt"
  )

  expect_output_file_opts(
    str(prt_cars, max.level = 1),
    "str/mtcars.txt"
  )

  expect_output_file_opts(
    str(prt_cars, give.length = TRUE),
    "str/mtcars-len.txt"
  )
})

test_that("truncation of str output", {

  skip_on_r_lt_3_4()

  expect_output_file_opts(
    str(prt_cars, vec.len = 100),
    "str/mtcars-100.txt"
  )
})
