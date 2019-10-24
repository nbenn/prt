
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

  dir_1 <- mtcars_fst_file(tempfile(tmpdir = tmp))
  prt_1 <- new_prt(list.files(dir_1, full.names = TRUE))

  expect_is(prt_1, "prt")
  expect_true(is_prt(prt_1))
  expect_identical(length(prt_1), 1L)

  dir_2 <- mtcars_fst_file(tempfile(tmpdir = tmp), 2L)
  prt_2 <- new_prt(list.files(dir_2, full.names = TRUE))

  expect_is(prt_2, "prt")
  expect_true(is_prt(prt_2))
  expect_identical(length(prt_2), 2L)

})
