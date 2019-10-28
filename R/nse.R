
#' NSE subsetting operations
#'
#' @name nse
#'
#' @inheritParams base::subset
#'
#' @export
subset.prt <- function(x, subset, select, drop = FALSE, ...) {

  if (!isFALSE(drop)) warning("Ignoring `drop` argument.")
  if (...length() > 0L) warning("Ignoring `...` arguments.")

  if (missing(subset)) subset <- NULL
  else subset <- substitute(subset)

  if (missing(select)) select <- NULL
  else select <- substitute(select)

  subset_quo(x, i = subset, j = select)
}

subset_quo <- function(x, i = NULL, j = NULL) {

  cols <- as.list(seq_along(x))
  names(cols) <- colnames(x)
  cols <- eval(j, cols)

  if (!is.null(cols)) cols <- vec_as_col_index(cols, x)

  if (is.null(i)) {

    prt_read(x, rows = NULL, columns = cols)

  } else {

    need_cols <- match(all.vars(i), colnames(x))

    if (all(is.na(need_cols))) {

      rows <- eval(i, list())
      rows <- vec_as_row_index(rows, x)

      prt_read(x, rows = rows, columns = cols)

    } else if (!any(is.na(need_cols))) {

      res <- prt_lapply(x, subset_fst, cols = need_cols, i = i, j = cols)
      data.table::rbindlist(res)

    } else {

      subset_prt(x, cols = need_cols[!is.na(need_cols)], i = i, j = cols)
    }
  }
}

subset_prt <- function(x, cols, i = NULL, j = NULL) {

  tmp <- prt_read(x, rows = NULL, columns = cols)

  rows <- eval(i, tmp)
  rows <- vec_as_row_index(rows, x)

  prt_read(x, rows = rows, columns = j)
}

subset_fst <- function(x, cols, i = NULL, j = NULL) {

  tmp <- fst_read(x, columns = cols)

  rows <- eval(i, tmp)
  rows <- vec_as_row_index(rows, x)

  fst_read(x, rows = rows, columns = j)
}
