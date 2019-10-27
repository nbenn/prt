
test_that("convert numeric index to partition number", {

  expect_identical(num_ind_to_part_no(1, 1), 1L)
  expect_identical(num_ind_to_part_no(1:3, 1), rep(1L, 3L))

  expect_identical(num_ind_to_part_no(1:3, c(5, 5)), c(1L, 1L, 1L))
  expect_identical(num_ind_to_part_no(4:7, c(5, 5)), c(1L, 1L, 2L, 2L))
  expect_identical(num_ind_to_part_no(7:4, c(5, 5)), c(2L, 2L, 1L, 1L))

  expect_identical(num_ind_to_part_no(4:7, c(5, 5, 5)),
                   num_ind_to_part_no(4:7, c(5, 5)))

  expect_identical(num_ind_to_part_no(c(3, 11), c(5, 5, 5)), c(1L, 3L))
  expect_identical(num_ind_to_part_no(c(3, 11), c(5, 5)), c(1L, 1L))

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

  expect_identical(num_ind_to_part_ind(4:7, c(5, 5)),
    num_ind_to_part_ind(4:7, c(5, 5),
                        part_no = num_ind_to_part_no(4:7, c(5, 5)))
  )

  expect_identical(num_ind_to_part_ind(c(3, 11), c(5, 5, 5)), c(3L, 1L))
  expect_identical(num_ind_to_part_ind(c(3, 11), c(5, 5)), c(3L, 11L))

  expect_error(num_ind_to_part_ind(c(-3, 4), c(5, 5)))
  expect_error(num_ind_to_part_ind(c(0, 4), c(5, 5)))
  expect_error(num_ind_to_part_ind(c(1, 2), c(5, 5), 1))
})

tmp <- tempfile()

setup(dir.create(tmp))
teardown(unlink(tmp, recursive = TRUE))

test_that("read fst data", {

  ref <- demo_data_table(dataset = "mtcars")
  prt_1 <- create_prt(dat = demo_data_frame(dataset = "mtcars"), dir = tmp,
                      n_chunks = 1L)
  fst <- unclass(prt_1)[[1L]]

  ind <- sample(nrow(ref), 5L)

  expect_identical(fst_read(fst, 1:3), ref[1:3, ])
  expect_identical(fst_read(fst, 3:1), ref[3:1, ])
  expect_identical(fst_read(fst, ind), ref[ind, ])
  expect_identical(fst_read(fst, NULL), ref)
  expect_identical(fst_read(fst, integer()), ref[integer(), ])

  cols <- c("disp", "drat")

  expect_identical(fst_read(fst, 1:3, cols), ref[1:3, cols, with = FALSE])
  expect_identical(fst_read(fst, 3:1, cols), ref[3:1, cols, with = FALSE])
  expect_identical(fst_read(fst, ind, cols), ref[ind, cols, with = FALSE])
  expect_identical(fst_read(fst, NULL, cols), ref[, cols, with = FALSE])
  expect_identical(fst_read(fst, integer(), cols),
                   ref[integer(), cols, with = FALSE])

  cols <- c("disp")

  expect_identical(fst_read(fst, 1:3, cols), ref[1:3, cols, with = FALSE])
  expect_identical(fst_read(fst, 3:1, cols), ref[3:1, cols, with = FALSE])
  expect_identical(fst_read(fst, ind, cols), ref[ind, cols, with = FALSE])
  expect_identical(fst_read(fst, NULL, cols), ref[, cols, with = FALSE])
  expect_identical(fst_read(fst, integer(), cols),
                   ref[integer(), cols, with = FALSE])

  expect_error(fst_read(fst, 1:3, "cols"))
  expect_error(fst_read(fst, 1:3, NA))

  expect_identical(fst_read(fst, 1:3, character()), data.table::data.table())
})

test_that("read prt data", {

  ref <- demo_data_table(dataset = "mtcars")
  prt_2 <- create_prt(dat = demo_data_frame(dataset = "mtcars"), dir = tmp,
                      n_chunks = 2L)

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
  expect_identical(fst_read(prt_2, 1:3, character()), data.table::data.table())
})

test_that("big_mark() works for large numbers", {
  expect_match(big_mark(123), "123")
  expect_match(big_mark(123456), "123.456")
  expect_match(big_mark(123456789), "123.456.789")
})
