
tmp <- tempfile()

setup(dir.create(tmp))
teardown(unlink(tmp, recursive = TRUE))

prt_cars <- create_prt(demo_data_frame("mtcars"), dir = tmp, n_chunks = 2L)
dt_cars <- demo_data_table("mtcars")

test_that("nse subsetting", {
  expect_identical(subset(prt_cars, 1:5), dt_cars[1:5, ])
  expect_identical(subset(prt_cars, mpg < 20), subset(dt_cars, mpg < 20))
  expect_identical(subset(prt_cars, mpg < 20, c("cyl", "disp")),
                   subset(dt_cars, mpg < 20, c("cyl", "disp")))
  expect_identical(subset(prt_cars, mpg < 20, c(cyl, disp)),
                   subset(dt_cars, mpg < 20, c(cyl, disp)))
  expect_identical(subset(prt_cars, mpg < 20, -c(cyl, disp)),
                   subset(dt_cars, mpg < 20, -c(cyl, disp)))
})
