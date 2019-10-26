
mtcars_data_frame <- function() {
  env <- new.env()
  name <- utils::data("mtcars", package = "datasets", envir = env)
  res <- env[[name]]
  rownames(res) <- NULL
  res
}

mtcars_data_table <- function() {
  data.table::setDT(data.table::copy(mtcars_data_frame()))
}

mtcars_tibble <- function() {
  tibble::as_tibble(mtcars_data_frame())
}

mtcars_fst_file <- function(dir = tempdir(), n_chunks = 1L,
                            dat = mtcars_data_frame()) {

  if (!dir.exists(dir)) dir.create(dir)

  Map(fst::write_fst, split(dat, prt:::split_indices(nrow(dat), n_chunks)),
      file.path(dir, paste0(seq_len(n_chunks), ".fst")))

  dir
}
