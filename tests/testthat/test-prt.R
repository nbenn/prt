
tmp <- tempfile()

setup(dir.create(tmp))
teardown(unlink(tmp, recursive = TRUE))

test_that("prt creation/conversion", {

  ref <- demo_data_frame(dataset = "mtcars")

  dir_1 <- fst_files(dat = ref, dir = tempfile(tmpdir = tmp), n_chunks = 1L)
  prt_1 <- new_prt(list.files(dir_1, full.names = TRUE))

  expect_is(prt_1, "prt")
  expect_true(is_prt(prt_1))
  expect_identical(n_part(prt_1), 1L)

  expect_identical(dim(prt_1), dim(ref))
  expect_identical(ncol(prt_1), ncol(ref))
  expect_identical(nrow(prt_1), nrow(ref))
  expect_identical(length(prt_1), ncol(prt_1))

  expect_identical(dimnames(prt_1), list(NULL, colnames(ref)))
  expect_identical(colnames(prt_1), colnames(ref))
  expect_identical(rownames(prt_1), NULL)
  expect_identical(names(prt_1), names(ref))

  expect_identical(as.data.table(prt_1), as.data.table(ref))
  expect_identical(as.list(prt_1), as.list(ref))
  expect_identical(as.data.frame(prt_1), as.data.frame(ref))

  expect_identical(as.matrix(prt_1), as.matrix(ref))
  expect_identical(ref, as.data.frame(
    as_prt(ref, dir = tempfile(tmpdir = tmp))
  ))
  expect_identical(ref, as.data.frame(
    as_prt(ref, dir = tempfile(tmpdir = tmp), n_chunks = 1L)
  ))

  lst_1 <- list(ref)

  expect_identical(ref, as.data.frame(
    as_prt(lst_1, dir = tempfile(tmpdir = tmp))
  ))
  expect_identical(ref, as.data.frame(
    as_prt(lst_1, dir = tempfile(tmpdir = tmp), n_chunks = 1L)
  ))
  expect_error(as_prt(lst_1, dir = tempfile(tmpdir = tmp), n_chunks = 2L))

  dir_2 <- fst_files(dat = ref, dir = tempfile(tmpdir = tmp), n_chunks = 2L)
  prt_2 <- new_prt(list.files(dir_2, full.names = TRUE))

  expect_is(prt_2, "prt")
  expect_true(is_prt(prt_2))
  expect_identical(n_part(prt_2), 2L)

  expect_identical(dim(prt_2), dim(ref))
  expect_identical(ncol(prt_2), ncol(ref))
  expect_identical(nrow(prt_2), nrow(ref))
  expect_identical(length(prt_2), ncol(prt_2))

  expect_identical(dimnames(prt_2), list(NULL, colnames(ref)))
  expect_identical(colnames(prt_2), colnames(ref))
  expect_identical(rownames(prt_2), NULL)
  expect_identical(names(prt_1), names(ref))

  expect_identical(as.data.table(prt_1), as.data.table(ref))
  expect_identical(as.list(prt_1), as.list(ref))
  expect_identical(as.data.frame(prt_1), as.data.frame(ref))

  expect_identical(as.matrix(prt_2), as.matrix(ref))
  expect_identical(ref, as.data.frame(
    as_prt(ref, dir = tempfile(tmpdir = tmp), n_chunks = 2L)
  ))

  lst_2 <- split(ref, split_indices(nrow(ref), 2L))

  expect_identical(ref, as.data.frame(
    as_prt(lst_2, dir = tempfile(tmpdir = tmp))
  ))
  expect_identical(ref, as.data.frame(
    as_prt(lst_2, dir = tempfile(tmpdir = tmp), n_chunks = 2L)
  ))
  expect_error(as_prt(lst_2, dir = tempfile(tmpdir = tmp), n_chunks = 1L))

  expect_warning(as.data.table(prt_1, foo = "bar"),
                 class = "warn_ignore_...")
  expect_warning(as.list(prt_1, foo = "bar"),
                 class = "warn_ignore_...")
  expect_warning(as.data.frame(prt_1, foo = "bar"),
                 class = "warn_ignore_...")
  expect_warning(as.data.frame(prt_1, row.names = rownames(mtcars)),
                 class = "warn_ignore_row.names")
  expect_warning(as.data.frame(prt_1, optional = TRUE),
                 class = "warn_ignore_optional")
  expect_warning(as.matrix(prt_1, foo = "bar"),
                 class = "warn_ignore_...")
})

test_that("file_fst head/tail", {

  ref <- demo_data_table(dataset = "mtcars")

  dat <- create_prt(dat = demo_data_frame(dataset = "mtcars"), dir = tmp,
                    n_chunks = 2L)

  expect_identical(head(dat), head(ref))
  expect_identical(head(dat, n = 10L), head(ref, n = 10L))
  expect_identical(head(dat, n = nrow(ref)), head(ref, n = nrow(ref)))
  expect_identical(head(dat, n = 100L), head(ref, n = 100L))
  expect_identical(head(dat, n = -1L), head(ref, n = -1L))
  expect_identical(head(dat, n = 0), head(ref, n = 0))
  expect_identical(head(dat, n = Inf), head(ref, n = Inf))
  expect_identical(head(dat, n = -Inf), head(ref, n = -Inf))
  expect_identical(head(dat, n = "foo"), head(ref, n = "foo"))

  expect_error(head(dat, n = NA))
  expect_error(head(ref, n = NA))
  expect_error(head(dat, n = c(1L, 2L)))
  expect_error(head(ref, n = c(1L, 2L)))

  expect_identical(tail(dat), tail(ref))
  expect_identical(tail(dat, n = 10L), tail(ref, n = 10L))
  expect_identical(tail(dat, n = nrow(ref)), tail(ref, n = nrow(ref)))
  expect_identical(tail(dat, n = 100L), tail(ref, n = 100L))
  expect_identical(tail(dat, n = -1L), tail(ref, n = -1L))
  expect_identical(tail(dat, n = 0), tail(ref, n = 0))
  expect_identical(tail(dat, n = Inf), tail(ref, n = Inf))
  expect_identical(tail(dat, n = -Inf), tail(ref, n = -Inf))
  expect_identical(tail(dat, n = "foo"), tail(ref, n = "foo"))

  expect_error(tail(dat, n = NA))
  expect_error(tail(ref, n = NA))
  expect_error(tail(dat, n = c(1L, 2L)))
  expect_error(tail(ref, n = c(1L, 2L)))
})
