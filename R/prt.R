
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

  make_prt(lapply(files, fst::fst))
}

make_prt <- function(x) {

  assert_that(is.list(x), all(vapply(x, inherits, logical(1L), "fst_table")))

  cols <- lapply(x, colnames)

  assert_that(all(vapply(cols, identical, logical(1L), cols[[1L]])))

  structure(x, class = "prt")
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

#' @rdname new_prt
#'
#' @export
#'
dim.prt <- function(x) {
  as.integer(c(sum(prt_nrows(x)), ncol(.subset2(x, 1L))))
}

#' @rdname new_prt
#'
#' @export
#'
dimnames.prt <- function(x) {
  list(NULL, colnames(.subset2(x, 1L)))
}

#' @param ... Generic consistency: additional arguments are ignored and a
#' warning is issued.
#'
#' @rdname new_prt
#'
#' @importFrom data.table as.data.table
#'
#' @export
#'
as.data.table.prt <- function(x, ...) {

  if (dots_n() > 0L) warning("Ignoring further `...` arguments.")

  prt_read(x, rows = NULL, columns = NULL)
}

#' @rdname new_prt
#'
#' @export
#'
as.list.prt <- function(x, ...) {

  if (dots_n() > 0L) warning("Ignoring further `...` arguments.")

  c(as.data.table(x))
}

#' @param row.names,optional Generic consistency: passing anything other than
#' the default value issues a warning.
#'
#' @rdname new_prt
#'
#' @export
#'
as.data.frame.prt <- function(x, row.names = NULL, optional = FALSE, ...) {

  if (!is.null(row.names)) warning("Ignoring `row.names` argument.")
  if (!isFALSE(optional)) warning("Ignoring `optional` argument.")
  if (dots_n() > 0L) warning("Ignoring further `...` arguments.")

  res <- data.table::setDF(as.data.table(x))
  res
}

#' @rdname new_prt
#'
#' @export
#'
as.matrix.prt <- function(x, ...) {

  if (dots_n() > 0L) warning("Ignoring further `...` arguments.")

  as.matrix(as.data.table(x))
}

#' @param n Count variable indicating the number of rows to return.
#'
#' @rdname new_prt
#'
#' @importFrom utils head
#'
#' @export
#'
head.prt <- function(x, n = 6L, ...) {

  assert_that(length(n) == 1L)

  if (n < 0L) n <- max(nrow(x) + n, 0L)
  else n <- min(n, nrow(x))

  prt_read(x, rows = seq_len(n), columns = NULL)
}

#' @rdname new_prt
#'
#' @importFrom utils tail
#'
#' @export
#'
tail.prt <- function(x, n = 6L, ...) {

  assert_that(length(n) == 1L)

  nrx <- nrow(x)

  if (n < 0L) n <- max(nrx + n, 0L)
  else n <- min(n, nrx)

  prt_read(x, rows = seq.int(to = nrx, length.out = n), columns = NULL)
}
