
output_file <- function(filename) file.path("output", filename)

expect_output_file_opts <- function(x, filename, ...) {
  withr::with_options(
    list(crayon.enabled = FALSE, ...),
    expect_output_file(x, output_file(filename), update = FALSE)
  )
}

skip_on_non_utf8_locale <- function() {
  if (!l10n_info()$"UTF-8") {
    skip("Non-UTF-8 locale")
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
