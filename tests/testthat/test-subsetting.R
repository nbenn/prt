
tmp <- tempfile()

setup(dir.create(tmp))
teardown(unlink(tmp, recursive = TRUE))

ref <- demo_data_table(dataset = "mtcars")
dat <- create_prt(dat = demo_data_frame(dataset = "mtcars"), dir = tmp,
                  n_chunks = 2L)

test_that("[[ subsetting ignores exact argument", {
  expect_warning(dat[["disp"]], NA)
  expect_warning(dat[["disp", exact = FALSE]], class = "warn_ignore_exact")
  expect_identical(getElement(dat, "disp"), ref[["disp"]])
})

test_that("can use recursive indexing with [[", {
  expect_identical(dat[[c(1, 1)]], ref[1, 1][[1L]])
  expect_identical(dat[[c(1, 2)]], ref[2, 1][[1L]])
  expect_error(dat[[c(1, 2, 3)]])
  expect_error(dat[[c(1, NA)]])
  expect_error(dat[[c(NA, 1)]])
  expect_error(dat[[c(NA, NA)]])
})

test_that("[[ subsetting with matrix index", {
  expect_warning(res <- dat[[matrix(1:6, ncol = 2L)]],
                 class = "warn_mat_subset")
  expect_identical(res, as.data.frame(ref)[matrix(1:6, ncol = 2L)])
  expect_error(dat[[matrix(1:6, ncol = 3L)]])
})

test_that("[[ returns NULL if name doesn't exist", {
  expect_null(dat[["y"]])
  expect_null(dat[[1, "y"]])

  expect_warning(res <- dat[[NA]], class = "warn_na_subset")
  expect_null(res)
})

test_that("can use two-dimensional indexing with [[", {
  expect_equal(dat[[1, 2]], ref[[1, 2]])
  expect_equal(dat[[2, 3]], ref[[2, 3]])

  expect_warning(res <- dat[[2, NA]], class = "warn_na_subset")
  expect_null(res)

  expect_warning(res <- dat[[NA, NA]], class = "warn_na_subset")
  expect_null(res)

  expect_error(dat[[NA, 1]])
})

test_that("$ throws warning if name doesn't exist", {
  expect_warning(expect_null(dat$y), class = "warn_miss_col")
})

test_that("$ doesn't do partial matching", {
  expect_warning(
    expect_null(dat$d), class = "warn_miss_col"
  )
  expect_warning(
    expect_null(dat$dis), class = "warn_miss_col"
  )
  expect_error(dat$disp, NA)
})

test_that("[ never drops", {
  expect_is(dat[1:5, ], "data.frame")
  expect_is(dat[, 1:5], "data.frame")
  expect_is(dat[1:5, 1:5], "data.frame")
  expect_is(dat[, 1], "data.frame")
  expect_equal(dat[, 1], dat[1])
})

test_that("[ with 0 cols returns NULL data.table", {
  expect_identical(dat[0], data.table::data.table())
  expect_identical(dat[, 0], data.table::data.table())
})

test_that("[ is careful about names", {
  expect_error(dat["z"])
  expect_error(dat[c("hp", "wt", "z")])
})

test_that("[ is careful about column indexes", {

  expect_identical(dat[seq_along(ref)], ref)

  expect_error(dat[0.5])
  expect_error(dat[seq_len(ncol(ref) + 1L)])

  expect_error(dat[-1:1])
  expect_error(dat[c(-1, 1)])

  expect_error(dat[-(ncol(ref) + 1L)])
  expect_error(dat[c(1:3, NA)])
})

test_that("[ is careful about column flags", {
  expect_identical(dat[TRUE], ref)
  expect_identical(dat[rep(TRUE, ncol(ref))], ref)
  expect_identical(dat[FALSE], data.table::data.table())
  expect_identical(dat[c(FALSE, TRUE, rep(FALSE, ncol(ref) - 2L))], dat[2])

  expect_error(dat[c(TRUE, TRUE)])
  expect_error(dat[c(rep(TRUE, ncol(ref)), FALSE)])
  expect_error(dat[c(rep(TRUE, ncol(ref) - 1L), NA)])
})

test_that("[ rejects unknown column indexes", {
  expect_error(dat[list(1:3)])
  expect_error(dat[as.list(1:3)])
  expect_error(dat[factor(1:3)])
  expect_error(dat[Sys.Date()])
  expect_error(dat[Sys.time()])
})

test_that("[ row subsetting", {
  expect_identical(dat[2:4, ], ref[2:4, ])
  expect_identical(dat[-3:-5, ], dat[c(1:2, seq.int(6, nrow(ref))), ])

  expect_identical(dat[c(9:10, NA, NA), ], ref[c(9:10, NA, NA), ])

  expect_warning(res <- dat[seq.int(nrow(dat) - 2L, nrow(dat) + 2L), ],
                 class = "warn_oob_ind")
  expect_identical(res, ref[seq.int(nrow(dat) - 2L, nrow(dat) + 2L), ])

  expect_warning(res <- dat[-seq.int(nrow(dat) - 2L, nrow(dat) + 2L), ],
                 class = "warn_oob_neg")
  expect_identical(res, ref[seq.int(1L, nrow(dat) - 3L), ])

  expect_error(dat[as.character(2:4), ])
})

test_that("[ supports logical subsetting", {
  expect_identical(dat[c(FALSE, rep(TRUE, 3), rep(FALSE, nrow(ref) - 4L)), ],
                   ref[2:4, ])
  expect_identical(dat[TRUE, ], ref)
  expect_identical(dat[FALSE, ], ref[0L, ])

  expect_warning(dat[c(TRUE, FALSE), ], class = "warn_ind_rep")
})

test_that("[ is no-op if args missing", {
  expect_identical(dat[], ref)
})

test_that("[ supports drop argument", {
  expect_identical(dat[1, 2, drop = TRUE], dat[[2]][1])
  expect_identical(dat[1, , drop = TRUE], dat[1, , ])

  expect_warning(res <- dat[1, drop = TRUE], class = "warn_ignore_drop")
  expect_identical(res, dat[1])
})
