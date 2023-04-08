
#' Methods for creating and inspecting prt objects
#'
#' The constructor `new_prt()` creates a `prt` object from one or several
#' `fst` files, making sure that each table consist of identically named,
#' ordered and typed columns. In order to create a `prt` object from an
#' in-memory table, `as_prt()` coerces objects inheriting from `data.frame`
#' to `prt` by first splitting rows into `n_chunks`, writing `fst` files to the
#' directory `dir` and calling `new_prt()` on the resulting `fst` files. If
#' this default splitting of rows (which might impact efficiency of subsequent
#' queries on the data) is not optimal, a list of objects inheriting from
#' `data.frame` is a valid `x` argument as well.
#'
#' To check whether an object inherits from `prt`, the function `is_prt()` is
#' exported, the number of partitions can be queried by calling `n_part()` and
#' the number of rows per partition is available as `part_nrow()`.
#'
#' The base `R` S3 generic functions [dim()], [length()], [dimnames()] and
#' [names()],have `prt`-specific implementations, where [dim()] returns the
#' overall table dimensions, [length()] is synonymous for [ncol()],
#' [dimnames()] returns a length 2 list containing `NULL` column names as
#' character vector and [names()] is synonymous for [colnames()]. Both setting
#' and getting row names on `prt` objects is not supported and more generally,
#' calling replacement functions such as `names<-()` or `dimnames<-()` leads
#' to an error, as `prt` objects are immutable. The base `R` S3 generic
#' functions [head()] and [tail()] are available as well and are used
#' internally to provide an extensible mechanism for printing (see
#' [format_dt()]).
#'
#' Coercion to other base `R` objects is possible via [as.list()],
#' [as.data.frame()] and [as.matrix()] and for coercion to `data.table`, its
#' generic function [data.table::as.data.table()] is available to `prt`
#' objects. All coercion involves reading the full data into memory at once
#' which might be problematic in cases of large data sets.
#'
#' @param files Character vector of file name(s).
#'
#' @examples
#' cars <- as_prt(mtcars, n_chunks = 2L)
#'
#' is_prt(cars)
#' n_part(cars)
#' part_nrow(cars)
#'
#' nrow(cars)
#' ncol(cars)
#'
#' colnames(cars)
#' names(cars)
#'
#' head(cars)
#' tail(cars, n = 2)
#'
#' str(as.list(cars))
#' str(as.data.frame(cars))
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

  all_ident <- function(x) all(vapply(x, identical, logical(1L), x[[1L]]))

  assert_that(is.list(x), all(vapply(x, inherits, logical(1L), "fst_table")))

  meta <- lapply(x, .subset2, "meta")

  assert_that(all(vapply(meta, inherits, logical(1L), "fstmetadata")),
              all_ident(lapply(meta, `[[`, "columnNames")),
              all_ident(lapply(meta, `[[`, "columnBaseTypes")),
              all_ident(lapply(meta, `[[`, "columnTypes")))

  structure(x, class = "prt")
}

#' @param x An object inheriting from [base::data.frame()] (requires both a
#' [base::nrow()] and [base::split()] method), or a list of objects inheriting
#' from [base::data.frame()].
#' @param n_chunks Count variable specifying the number of chunks `x` is
#' split into.
#' @param dir Directory where the chunked [fst::fst()] objects reside in.
#'
#' @rdname new_prt
#'
#' @export
#'
as_prt <- function(x, n_chunks = NULL, dir = tempfile()) {

  assert_that(is.string(dir))

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  is_df <- inherits(x, "data.frame")

  if (is.null(n_chunks) && is_df) {

    n_chunks <- 1L

  } else if (is_df) {

    assert_that(is.count(n_chunks))

  } else {

    assert_that(is.list(x), length(x) > 0L,
                all(vapply(x, inherits, logical(1L), "data.frame")))

    len <- length(x)

    if (is.null(n_chunks)) {
      n_chunks <- len
    } else {
      assert_that(isTRUE(all.equal(n_chunks, len)))
    }
  }

  assert_that(is.string(dir))

  filenames <- file.path(dir, paste0(seq_len(n_chunks), ".fst"))

  assert_that(!any(file.exists(filenames)))

  if (is_df) {
    x <- split(x, split_indices(nrow(x), n_chunks))
  }

  Map(fst::write_fst, x, filenames)

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
n_part <- function(x) {
  length(unclass(x))
}

#' @rdname new_prt
#'
#' @export
#'
part_nrow <- function(x) prt_vapply(x, nrow, numeric(1L))

#' @export
dim.prt <- function(x) {
  as.integer(c(sum(part_nrow(x)), ncol(.subset2(x, 1L))))
}

#' @export
length.prt <- function(x) ncol(x)

#' @export
dimnames.prt <- function(x) {
  list(NULL, colnames(.subset2(x, 1L)))
}

abort_immutable <- function(..., value) {
  abort("`prt` objects are immutable", "err_immutable")
}

#' @export
`dimnames<-.prt` <- abort_immutable

#' @export
names.prt <- function(x) colnames(x)

#' @export
`names<-.prt` <- abort_immutable

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

  if (n == nrow(x)) rows <- NULL
  else rows <- seq_len(n)

  prt_read(x, rows = rows, columns = NULL)
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

  if (n == nrx) rows <- NULL
  else rows <- seq.int(to = nrx, length.out = n)

  prt_read(x, rows = rows, columns = NULL)
}

#' @param ... Generic consistency: additional arguments are ignored and a
#' warning is issued.
#'
#' @rdname new_prt
#'
#' @importFrom data.table as.data.table
#' @method as.data.table prt
#'
#' @export
#'
as.data.table.prt <- function(x, ...) {

  if (...length() > 0L) {
    warn_arg("...")
  }

  prt_read(x, rows = NULL, columns = NULL)
}

#' @rdname new_prt
#'
#' @method as.list prt
#'
#' @export
#'
as.list.prt <- function(x, ...) {

  if (...length() > 0L) {
    warn_arg("...")
  }

  c(as.data.table(x))
}

#' @param row.names,optional Generic consistency: passing anything other than
#' the default value issues a warning.
#'
#' @rdname new_prt
#'
#' @method as.data.frame prt
#'
#' @export
#'
as.data.frame.prt <- function(x, row.names = NULL, optional = FALSE, ...) {

  if (!is.null(row.names)) {
    warn_arg("row.names")
  }

  if (!isFALSE(optional)) {
    warn_arg("optional")
  }

  if (...length() > 0L) {
    warn_arg("...")
  }

  res <- data.table::setDF(as.data.table(x))
  res
}

#' @rdname new_prt
#'
#' @method as.matrix prt
#'
#' @export
#'
as.matrix.prt <- function(x, ...) {

  if (...length() > 0L) {
    warn_arg("...")
  }

  as.matrix(as.data.table(x))
}
