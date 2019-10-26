
tmp <- tempfile()

setup(dir.create(tmp))
teardown(unlink(tmp, recursive = TRUE))

ref <- mtcars_tibble()

dir <- mtcars_fst_file(tempfile(tmpdir = tmp), 2L)
dat <- new_prt(list.files(dir, full.names = TRUE))

test_that("[[ subsetting ignores exact argument", {
  expect_warning(dat[["disp"]], NA)
  expect_warning(dat[["disp", exact = FALSE]], "ignored")
  expect_identical(getElement(dat, "disp"), ref[["disp"]])
})

test_that("can use recursive indexing with [[", {
  expect_identical(dat[[c(1, 1)]], ref[1, 1][[1L]])
  expect_identical(dat[[c(1, 2)]], ref[2, 1][[1L]])
  expect_error(dat[[c(1, 2, 3)]])
  expect_error(dat[[c(1, NA)]])
})

test_that("[[ subsetting with matrix index", {
  expect_warning(res <- dat[[matrix(1:6, ncol = 2L)]])
  expect_identical(res, as.data.frame(ref)[matrix(1:6, ncol = 2L)])
  expect_error(dat[[matrix(1:6, ncol = 3L)]])
})

test_that("[[ returns NULL if name doesn't exist", {
  expect_null(dat[["y"]])
  expect_null(dat[[1, "y"]])
})

test_that("can use two-dimensional indexing with [[", {
  expect_equal(dat[[1, 2]], ref[[1, 2]])
  expect_equal(dat[[2, 3]], ref[[2, 3]])
})

test_that("$ throws warning if name doesn't exist", {
  expect_warning(
    expect_null(dat$y),
    "Unknown or uninitialised column: `y`",
    fixed = TRUE
  )
})

test_that("$ doesn't do partial matching", {
  expect_warning(
    expect_null(dat$d),
    "Unknown or uninitialised column: `d`",
    fixed = TRUE
  )
  expect_warning(
    expect_null(dat$dis),
    "Unknown or uninitialised column: `dis`",
    fixed = TRUE
  )
  expect_error(dat$disp, NA)
})

