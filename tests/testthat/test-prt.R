
mtcars_data_frame <- function() {
  env <- new.env()
  name <- utils::data("mtcars", package = "datasets", envir = env)
  env[[name]]
}

mtcars_fst_file <- function(dir = tempdir(), n_chunks = 1L,
                            dat = mtcars_data_frame()) {

  if (!dir.exists(dir)) dir.create(dir)

  Map(fst::write_fst, split(dat, prt:::split_indices(nrow(dat), n_chunks)),
      file.path(dir, paste0(seq_len(n_chunks), ".fst")))

  dir
}

tmp <- tempfile()

setup({
  dir.create(tmp)
})

teardown(unlink(tmp, recursive = TRUE))

test_that("prt creation", {

  ref <- mtcars_data_frame()

  dir_1 <- mtcars_fst_file(tempfile(tmpdir = tmp))
  prt_1 <- new_prt(list.files(dir_1, full.names = TRUE))

  expect_is(prt_1, "prt")
  expect_true(is_prt(prt_1))
  expect_identical(length(prt_1), 1L)

  expect_identical(dim(prt_1), dim(ref))
  expect_identical(ncol(prt_1), ncol(ref))
  expect_identical(nrow(prt_1), nrow(ref))

  expect_identical(dimnames(prt_1), list(NULL, colnames(ref))))
  expect_identical(colnames(prt_1), colnames(ref))
  expect_identical(rownames(prt_1), NULL)

  expect_identical(data.table::as.data.table(prt_1),
                   data.table::as.data.table(ref))
  expect_identical(as.list(prt_1), as.list(ref))
  expect_identical(as.data.frame(prt_1), as.data.frame(ref))

  dir_2 <- mtcars_fst_file(tempfile(tmpdir = tmp), 2L)
  prt_2 <- new_prt(list.files(dir_2, full.names = TRUE))

  expect_is(prt_2, "prt")
  expect_true(is_prt(prt_2))
  expect_identical(length(prt_2), 2L)

  expect_identical(dimnames(prt_2), list(NULL, colnames(ref))))
  expect_identical(colnames(prt_2), colnames(ref))
  expect_identical(rownames(prt_2), NULL)

  expect_identical(data.table::as.data.table(prt_1),
                   data.table::as.data.table(ref))
  expect_identical(as.list(prt_1), as.list(ref))
  expect_identical(as.data.frame(prt_1), as.data.frame(ref))
})
