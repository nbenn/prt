
output_file <- function(filename) file.path("output", filename)

expect_output_file_opts <- function(x, filename, ...) {
  withr::with_options(
    list(crayon.enabled = FALSE, ...),
    expect_known_output(x, output_file(filename), update = FALSE)
  )
}

skip_on_non_utf8_locale <- function() {
  if (!l10n_info()$"UTF-8") {
    skip("Non-UTF-8 locale")
  }
}

skip_on_r_lt_3_4 <- function() {
  if (getRversion() < "3.4.0") {
    skip("R version < 3.4.0")
  }
}

demo_data_frame <- function(dataset = "mtcars") {
  env <- new.env()
  name <- utils::data(list = dataset, package = "datasets", envir = env)
  res <- env[[name]]
  rownames(res) <- NULL
  res
}

demo_data_table <- function(dataset = "mtcars") {
  data.table::setDT(data.table::copy(demo_data_frame(dataset = dataset)))
}

demo_tibble <- function(dataset = "mtcars") {
  tibble::as_tibble(demo_data_frame(dataset = dataset))
}

fst_files <- function(dat = demo_data_frame(), dir = tempdir(),
                      n_chunks = 1L) {

  if (!dir.exists(dir)) dir.create(dir)

  Map(fst::write_fst, split(dat, prt:::split_indices(nrow(dat), n_chunks)),
      file.path(dir, paste0(seq_len(n_chunks), ".fst")))

  dir
}

create_prt <- function(dat = demo_data_frame(), dir = tempdir(),
                       n_chunks = 1L) {

  dir <- fst_files(dat = dat, dir = tempfile(tmpdir = dir),
                   n_chunks = n_chunks)

  new_prt(list.files(dir, full.names = TRUE))
}

df_all <- data.frame(
  a = c(1, 2.5, NA),
  b = c(1:2, NA),
  c = c(T, F, NA),
  d = I(c("a", "b", NA)),
  e = factor(c("a", "b", NA)),
  f = as.Date("2015-12-09") + c(1:2, NA),
  g = as.POSIXct("2015-12-09 10:51:34 UTC") + c(1:2, NA)
)
