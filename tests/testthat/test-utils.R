
test_that("convert numeric index to partition number", {

  expect_identical(num_ind_to_part_no(1, 1), 1L)
  expect_identical(num_ind_to_part_no(1:3, 1), rep(1L, 3L))

  expect_identical(num_ind_to_part_no(1:3, c(5, 5)), c(1L, 1L, 1L))
  expect_identical(num_ind_to_part_no(4:7, c(5, 5)), c(1L, 1L, 2L, 2L))
  expect_identical(num_ind_to_part_no(7:4, c(5, 5)), c(2L, 2L, 1L, 1L))

  expect_identical(num_ind_to_part_no(4:7, c(5, 5, 5)),
                   num_ind_to_part_no(4:7, c(5, 5)))

  expect_identical(num_ind_to_part_no(c(3, 11), c(5, 5, 5)), c(1L, 3L))

  expect_error(num_ind_to_part_no(c(3, 11), c(5, 5)))
  expect_error(num_ind_to_part_no(c(-3, 4), c(5, 5)))
  expect_error(num_ind_to_part_no(c(0, 4), c(5, 5)))
})

test_that("convert numeric index to partition index", {

  expect_identical(num_ind_to_part_ind(1, 1), 1L)
  expect_identical(num_ind_to_part_ind(1:3, 1), 1L:3L)

  expect_identical(num_ind_to_part_ind(4:7, c(5, 5)), c(4L:5L, 1L:2L))
  expect_identical(num_ind_to_part_ind(c(6:7, 4:5), c(5, 5)), c(1L:2L, 4L:5L))
  expect_identical(num_ind_to_part_ind(7:4, c(5, 5)), c(2L:1L, 5L:4L))

  expect_identical(num_ind_to_part_ind(4:7, c(5, 5, 5)),
                   num_ind_to_part_ind(4:7, c(5, 5)))

  expect_identical(num_ind_to_part_ind(c(3, 11), c(5, 5, 5)), c(3L, 1L))

  expect_identical(num_ind_to_part_ind(4:7, c(5, 5)),
    num_ind_to_part_ind(4:7, c(5, 5),
                        part_no = num_ind_to_part_no(4:7, c(5, 5)))
  )

  expect_error(num_ind_to_part_ind(c(3, 11), c(5, 5)))
  expect_error(num_ind_to_part_ind(c(-3, 4), c(5, 5)))
  expect_error(num_ind_to_part_ind(c(0, 4), c(5, 5)))
  expect_error(num_ind_to_part_ind(c(1, 2), c(5, 5), 1))
})

tmp <- tempfile()

setup({
  dir.create(tmp)
})

teardown(unlink(tmp, recursive = TRUE))

test_that("read fst data", {

  ref <- mtcars_data_table()

  dir_1 <- mtcars_fst_file(tempfile(tmpdir = tmp))
  filename <- list.files(dir_1, full.names = TRUE)
  prt_1 <- new_prt(filename)

  ind <- sample(nrow(ref), 5L)

  expect_identical(fst_read(filename, 1:3), ref[1:3, ])
  expect_identical(fst_read(filename, 3:1), ref[3:1, ])
  expect_identical(fst_read(filename, ind), ref[ind, ])
  expect_identical(fst_read(filename, NULL), ref)
  expect_identical(fst_read(filename, integer()), ref[integer(), ])

  cols <- c("disp", "drat")

  expect_identical(fst_read(filename, 1:3, cols), ref[1:3, cols, with = FALSE])
  expect_identical(fst_read(filename, 3:1, cols), ref[3:1, cols, with = FALSE])
  expect_identical(fst_read(filename, ind, cols), ref[ind, cols, with = FALSE])
  expect_identical(fst_read(filename, NULL, cols), ref[, cols, with = FALSE])
  expect_identical(fst_read(filename, integer(), cols),
                   ref[integer(), cols, with = FALSE])

  cols <- c("disp")

  expect_identical(fst_read(filename, 1:3, cols), ref[1:3, cols, with = FALSE])
  expect_identical(fst_read(filename, 3:1, cols), ref[3:1, cols, with = FALSE])
  expect_identical(fst_read(filename, ind, cols), ref[ind, cols, with = FALSE])
  expect_identical(fst_read(filename, NULL, cols), ref[, cols, with = FALSE])
  expect_identical(fst_read(filename, integer(), cols),
                   ref[integer(), cols, with = FALSE])

  expect_error(fst_read(filename, 1:3, "cols"))
  expect_error(fst_read(filename, 1:3, NA))
  expect_error(fst_read(filename, 1:3, character()))
})

test_that("read prt data", {

  ref <- mtcars_data_table()

  dir_2 <- mtcars_fst_file(tempfile(tmpdir = tmp), 2L)
  filenames <- list.files(dir_2, full.names = TRUE)
  prt_2 <- new_prt(filenames)

  ind <- sample(nrow(ref), 10L)

  expect_identical(prt_read(prt_2), ref)
  expect_identical(prt_read(prt_2, 1:3), ref[1:3, ])
  expect_identical(prt_read(prt_2, 3:1), ref[3:1, ])
  expect_identical(prt_read(prt_2, 10:20), ref[10:20, ])
  expect_identical(prt_read(prt_2, 20:10), ref[20:10, ])
  expect_identical(prt_read(prt_2, ind), ref[ind, ])
  expect_identical(prt_read(prt_2, NULL), ref)
  expect_identical(prt_read(prt_2, integer()), ref[integer(), ])

  expect_identical(prt_read(prt_2, 1:3, c("disp", "drat")),
                   ref[1:3, c("disp", "drat"), with = FALSE])
  expect_identical(prt_read(prt_2, 1:3, "disp"),
                   ref[1:3, "disp", with = FALSE])

  expect_error(prt_read(prt_2, 1:3, "cols"))
  expect_error(prt_read(prt_2, 1:3, NA))
  expect_error(prt_read(prt_2, 1:3, character()))
})
