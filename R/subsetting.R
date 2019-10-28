
#' Subsetting operations
#'
#' @name subsetting
#'
#' @param x A `prt` object.
#' @param i,j Row/column indexes. If `j` is omitted, `i` is used as column
#' index.
#' @param exact Generic compatibility: only the default value of `TRUE` is
#' supported.
#' @param ... Generic compatibility: any further arguments are ignored.
#'
#' @export
`[[.prt` <- function(x, i, j, ..., exact = TRUE) {

  if (!isTRUE(exact)) {
    warning("`exact` ignored.")
  }

  n_dots <- ...length()

  if (n_dots > 0) {
    warning("Extra arguments ignored.")
  }

  n_real_args <- nargs() - !missing(exact) - n_dots

  if (n_real_args <= 2L) {

    prt_subset2(x, j = i, i = NULL)

  } else if (missing(j)) {

    stop("The column index `j` may not be missing if a row index `i` is ",
         "supplied.")

  } else {

    prt_subset2(x, j = j, i = i)
  }
}

#' @rdname subsetting
#'
#' @inheritParams base::`[.data.frame`
#'
#' @export
#'
`$.prt` <- function(x, name) {

  j <- match(name, colnames(x))

  if (is.na(j)) {
    warning("Unknown or uninitialised column: `", name, "`.")
    NULL
  } else {
    prt_subset2(x, j, i = NULL)
  }
}

#' @rdname subsetting
#'
#' @param drop Coerce to a vector if fetching one column via `tbl[, j]`.
#' Default `FALSE`, ignored when accessing a column via `tbl[j]`.
#'
#' @export
#'
`[.prt` <- function(x, i, j, drop = FALSE) {

  n_real_args <- nargs() - !missing(drop)

  if (n_real_args <= 2L) {

    if (!missing(drop)) warning("`drop` ignored")

    if (missing(i)) i <- NULL
    else i <- vec_as_col_index(i, x)

    res <- prt_read(x, rows = NULL, columns = i)

  } else {

    if (missing(i)) i <- NULL
    else i <- vec_as_row_index(i, x)

    if (missing(j)) j <- NULL
    else j <- vec_as_col_index(j, x)

    res <- prt_read(x, rows = i, columns = j)

    if (drop && ncol(res) == 1L) {
      res <- res[[1L]]
    }
  }

  res
}

prt_subset2 <- function(x, j, i = NULL) {

  if (is.matrix(j)) {

    assert_that(ncol(j) == 2L, is.numeric(j), is.null(i))

    warning("Single element subsetting with a matrix is not recommended ",
            "as the current implementation is inefficient.")

    rng_i <- range(j[, 1L])
    rng_j <- range(j[, 2L])

    res <- prt_read(x, rows = seq.int(rng_i[1L], rng_i[2L]),
                    columns = seq.int(rng_j[1L], rng_j[2L]))

    j[, 1L] <- 1L + j[, 1L] - rng_i[1L]
    j[, 2L] <- 1L + j[, 2L] - rng_j[1L]

    data.table::setDF(res)
    on.exit(data.table::setDT(res))

    return(res[j])

  } else if (length(j) == 2L && is.numeric(j)) {

    assert_that(is.null(i), !anyNA(j))

    res <- prt_read(x, rows = j[2L], columns = j[1L])
    return(res[[1L]])
  }

  assert_that(length(j) == 1L)

  if (is.na(j)) {
    warning("Single element subsetting with `NA` yields `NULL`")
    return(NULL)
  }

  if (is.logical(j)) {
    if (isTRUE(j)) j <- 1L
    else stop("Single element subsetting with logical values only supports ",
              "`TRUE`.")
  } else if (is.numeric(j)) {
    j <- vctrs::vec_as_index(j, nrow(x))
  } else if (is.character(j)) {
    j <- match(j, colnames(x))
    if (is.na(j)) return(NULL)
  }

  if (!is.null(i)) {
    i <- vec_as_row_index(i, x)
    assert_that(length(i) == 1L)
  }

  res <- prt_read(x, rows = i, columns = j)
  res[[1L]]
}

vec_as_col_index <- function(j, x) {

  stopifnot(!is.null(j))

  if (anyNA(j)) {
    pos <- paste(which(is.na(j)), collapse = ", ")
    stop("Can't use NA as column index with `[` at position(s) ", pos, ".")
  }

  vctrs::vec_as_index(j, ncol(x), colnames(x))
}

vec_as_row_index <- function(i, x) {

  stopifnot(!is.null(i))

  nr <- nrow(x)

  if (is.character(i)) {

    stop("Rownames are not supported by `prt` objects.")

  } else if (is.numeric(i)) {

    i <- fix_oob(i, nr)

  } else if (is.logical(i)) {

    if (length(i) != 1L && length(i) != nr) {
      warning("Length of logical index must be 1",
              if (nr != 1) paste0(" or ", nr),
              ", not ", length(i))
      return(seq_len(nr)[i])
    }
  }

  vctrs::vec_as_index(i, nr)
}

fix_oob <- function(i, n) {

  if (all(i >= 0, na.rm = TRUE)) {

    oob <- which(i > n)

    if (length(oob) > 0L) {
      warning("Row indexes must be between 0 and the number of rows (", n,
              "). Use `NA` as row index to obtain a row full of `NA` values.")
    }

    i[oob] <- NA_integer_

  } else if (all(i <= 0, na.rm = TRUE)) {

    oob <- (i < -n)

    if (length(which(oob)) > 0L) {
      warning("Negative row indexes must be between 0 and the number of rows ",
              "negated (", -n, "). Use `NA` as row index to obtain a row ",
              "full of `NA` values.")
    }

    i <- i[!oob]
    if (length(i) == 0L) i <- seq_len(n)
  }

  i
}
