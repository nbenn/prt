
test_that("format_row for values", {
  expect_equal(format_row(1), "1")
  expect_equal(format_row(1:3), c("1", "2", "3"))
  expect_equal(format_row(NA), "NA")
  expect_equal(format_row(TRUE), "TRUE")
  expect_equal(format_row(logical()), character())
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

df_all <- data.frame(
  a = c(1, 2.5, NA),
  b = c(1:2, NA),
  c = c(T, F, NA),
  d = I(c("a", "b", NA)),
  e = factor(c("a", "b", NA)),
  f = as.Date("2015-12-09") + c(1:2, NA),
  g = as.POSIXct("2015-12-09 10:51:34 UTC") + c(1:2, NA)
)

prt_all <- create_prt(df_all, dir = tmp)

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
