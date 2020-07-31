
#' Subsetting operations
#'
#' Both single element subsetting via `[[` and `$`, as well as multi-element
#' subsetting via `[` are available for `prt` objects. Subsetting semantics
#' are modeled after those of the `tibble` class with the main difference
#' being that there `tibble` returns `tibble` objects, `prt` returns
#' `data.table`s. Differences to base R include that partial column name
#' matching for `$` is not allowed and coercion to lower dimensions for
#' `[` is always disabled by default. As `prt` objects are immutable, all
#' subset-replace functions (`[[<-`, `$<-` and `[<-`) yield an error when
#' passed a `prt` object.
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
#' @examples
#' dat <- as_prt(mtcars)
#'
#' identical(dat$mpg, dat[["mpg"]])
#'
#' dat$mp
#' mtcars$mp
#'
#' identical(dim(dat["mpg"]), dim(mtcars["mpg"]))
#' identical(dim(dat[, "mpg"]), dim(mtcars[, "mpg"]))
#' identical(dim(dat[1L, ]), dim(mtcars[1L, ]))
#'
#' @export
#'
`[[.prt` <- function(x, i, j, ..., exact = TRUE) {

  if (!isTRUE(exact)) {
    warn_arg("exact")
  }

  n_dots <- ...length()

  if (n_dots > 0) {
    warn_arg("...")
  }

  n_real_args <- nargs() - !missing(exact) - n_dots

  if (n_real_args <= 2L) {

    prt_subset2(x, j = i, i = NULL)

  } else if (missing(j)) {

    abort(
      paste("The column index `j` may not be missing if a row index `i` is",
            "supplied."),
      "err_need_j_arg"
    )

  } else {

    prt_subset2(x, j = j, i = i)
  }
}

#' @export
`[[<-.prt` <- abort_immutable

#' @rdname subsetting
#'
#' @inheritParams base::`[.data.frame`
#'
#' @export
#'
`$.prt` <- function(x, name) {

  j <- match(name, colnames(x))

  if (is.na(j)) {
    warn(paste0("Unknown or uninitialised column: `", name, "`."),
         "warn_miss_col")
    NULL
  } else {
    prt_subset2(x, j, i = NULL)
  }
}

#' @export
`$<-.prt` <- abort_immutable

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

    if (!missing(drop)) {
      warn_arg("drop")
    }

    if (missing(i)) i <- NULL
    else i <- vec_as_col_index(i, colnames(x))

    res <- prt_read(x, rows = NULL, columns = i)

  } else {

    if (missing(i)) i <- NULL
    else i <- vec_as_row_index(i, nrow(x))

    if (missing(j)) j <- NULL
    else j <- vec_as_col_index(j, colnames(x))

    res <- prt_read(x, rows = i, columns = j)

    if (drop && ncol(res) == 1L) {
      res <- res[[1L]]
    }
  }

  res
}

#' @export
`[<-.prt` <- abort_immutable

prt_subset2 <- function(x, j, i = NULL) {

  if (is.matrix(j)) {

    assert_that(ncol(j) == 2L, is.numeric(j), is.null(i))

    warn(
      paste("Single element subsetting with a matrix is not recommended",
            "as the current implementation is inefficient."),
      "warn_mat_subset"
    )

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
    warn("Single element subsetting with `NA` yields `NULL`", "warn_na_subset")
    return(NULL)
  }

  if (is.logical(j)) {

    if (isTRUE(j)) {

      j <- 1L

    } else {

      abort(
        paste("Single element subsetting with logical values only supports",
              "`TRUE`."),
        "err_flag_subset"
      )
    }

  } else if (is.numeric(j)) {

    j <- vctrs::vec_as_location(j, nrow(x))

  } else if (is.character(j)) {

    j <- match(j, colnames(x))

    if (is.na(j)) {
      return(NULL)
    }
  }

  if (!is.null(i)) {
    i <- vec_as_row_index(i, nrow(x))
    assert_that(length(i) == 1L)
  }

  res <- prt_read(x, rows = i, columns = j)

  res[[1L]]
}

vec_as_col_index <- function(j, cols) {

  assert_that(!is.null(j))

  if (anyNA(j)) {

    pos <- paste(which(is.na(j)), collapse = ", ")

    abort(
      paste0("Can't use NA as column index with `[` at position(s) ",
             pos, "."),
      "err_na_subset"
    )
  }

  vctrs::vec_as_location(j, length(cols), cols)
}

vec_as_row_index <- function(i, n_row) {

  assert_that(!is.null(i))

  if (is.character(i)) {

    abort("Rownames are not supported by `prt` objects.", "err_rownames")

  } else if (is.numeric(i)) {

    i <- fix_oob(i, n_row)

  } else if (is.logical(i)) {

    if (length(i) != 1L && length(i) != n_row) {

      warn(
        paste0("Length of logical index must be 1",
               if (n_row != 1) paste0(" or ", n_row), ", not ", length(i)),
        "warn_ind_rep"
      )

      return(seq_len(n_row)[i])
    }
  }

  vctrs::vec_as_location(i, n_row)
}

fix_oob <- function(i, n) {

  if (all(i >= 0, na.rm = TRUE)) {

    oob <- which(i > n)

    if (length(oob) > 0L) {
      warn(
        paste0("Row indexes must be between 0 and the number of rows (", n,
               "). Use `NA` as row index to obtain a row full of `NA` ",
               "values."),
        "warn_oob_ind"
      )
    }

    i[oob] <- NA_integer_

  } else if (all(i <= 0, na.rm = TRUE)) {

    oob <- (i < -n)

    if (length(which(oob)) > 0L) {
      warn(
        paste0("Negative row indexes must be between 0 and the number of ",
               "rows negated (", -n, "). Use `NA` as row index to obtain a ",
               "row full of `NA` values."),
        "warn_oob_neg"
      )
    }

    i <- i[!oob]

    if (length(i) == 0L) {
      i <- seq_len(n)
    }
  }

  i
}
