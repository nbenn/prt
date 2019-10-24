
#' @importFrom assertthat assert_that is.count is.string
NULL

#' Methods for creating and inspecting prt objects
#'
#' @param files Character vector of file name(s).
#'
#' @export
#'
new_prt <- function(files) {

  assert_that(
    all(file.exists(files)), length(file) >= 1L
  )

  structure(lapply(files, fst::fst), class = "prt")
}

#' @param tbl An object inheriting from [base::data.frame()]. Requires both a
#' [base::nrow()] and [base::split()] method.
#' @param n_chunks Count variable specifying the number of chunks `tbl` is
#' split into.
#' @param dir Directory where the chunked [fst::fst()] objects reside in.
#'
#' @rdname new_prt
#'
#' @export
#'
as_prt <- function(tbl, n_chunks = 1L, dir = tempfile()) {

  assert_that(inherits(tbl, "data.frame"), is.count(n_chunks), is.string(dir))

  if (!dir.exists(dir)) dir.create(dir)

  filenames <- file.path(dir, paste0(seq_len(n_chunks), ".fst"))

  assert_that(!any(file.exists(filenames)))

  Map(fst::write_fst, split(tbl, split_indices(nrow(tbl), n_chunks)),
      filenames)

  new_prt(filenames)
}

#' @param x A `prt` object.
#'
#' @rdname new_prt
#'
#' @export
#'
is_prt <- function(x) inherits(x, "prt")

#' @rdname new_prt
#'
#' @export
#'
length.prt <- function(x) length(unclass(x))
