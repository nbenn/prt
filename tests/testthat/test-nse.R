
tmp <- tempfile()

setup(dir.create(tmp))
teardown(unlink(tmp, recursive = TRUE))

prt_cars <- create_prt(demo_data_frame("mtcars"), dir = tmp, n_chunks = 2L)
dt_cars <- demo_data_table("mtcars")
tbl_cars <- demo_tibble("mtcars")

test_that("nse subsetting", {

  expect_identical(
    subset(prt_cars, c(rep(TRUE, 5), rep(FALSE, nrow(mtcars) - 5))),
    subset(dt_cars, c(rep(TRUE, 5), rep(FALSE, nrow(mtcars) - 5)))
  )

  expect_warning(subset(prt_cars, c(T, T, F)), class = "warn_ind_rep")
  expect_error(subset(tbl_cars, c(T, T, F)),
               class = "vctrs_error_subscript_size")

  expect_error(subset(prt_cars, 1:5), class = "err_lgl_subset")

  expect_identical(subset(prt_cars, select = 1:5), dt_cars[, 1:5])

  inds <- 1:5
  expect_error(subset(prt_cars, inds), class = "err_lgl_subset")
  expect_identical(subset(prt_cars, select = inds),
                          dt_cars[, inds, with = FALSE])

  get_ind <- function(x = 5L) seq_len(x)
  expect_error(subset(prt_cars, get_ind()), class = "err_lgl_subset")
  expect_identical(subset(prt_cars, select = get_ind()),
                          dt_cars[, get_ind(), with = FALSE])

  expect_identical(subset(prt_cars, mpg < 20), subset(dt_cars, mpg < 20))
  expect_identical(subset(prt_cars, (mpg < 20) & (hp > 100)),
                   subset(dt_cars, (mpg < 20) & (hp > 100)))

  mpg_thresh <- 20
  expect_identical(subset(prt_cars, mpg < 20),
                   subset(dt_cars, mpg < mpg_thresh))
  expect_identical(subset(prt_cars, mpg < mpg_thresh, part_safe = TRUE),
                   subset(dt_cars, mpg < mpg_thresh))

  expect_identical(subset(prt_cars, mpg < 20), subset(dt_cars, mpg < 20))
  expect_identical(subset(prt_cars, mpg < 20, c("cyl", "disp")),
                   subset(dt_cars, mpg < 20, c("cyl", "disp")))
  expect_identical(subset(prt_cars, mpg < 20, c(cyl, disp)),
                   subset(dt_cars, mpg < 20, c(cyl, disp)))
  expect_identical(subset(prt_cars, mpg < 20, -c(cyl, disp)),
                   subset(dt_cars, mpg < 20, -c(cyl, disp)))

  inds <- sample(c(TRUE, FALSE), nrow(mtcars), replace = TRUE)
  expect_warning(subset(prt_cars, inds & mpg < 20, part_safe = TRUE),
                 class = "warn_ind_rep")
  expect_identical(subset(prt_cars, inds & mpg < 20, part_safe = FALSE),
                   subset(dt_cars, inds & mpg < 20))

  expect_identical(subset(prt_cars), subset(dt_cars))
  expect_identical(subset(prt_cars), dt_cars)

  expect_warning(subset(prt_cars, foo = "bar"), class = "warn_ignore_...")
  expect_warning(subset(prt_cars, drop = TRUE), class = "warn_ignore_drop")
})
