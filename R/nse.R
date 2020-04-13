
#' NSE subsetting operations
#'
#' @inheritParams base::subset
#'
#' @param part_safe Logical flax indicating whether the `subset` expression
#' can be safely be applied to individual partitions. The default is `FALSE`,
#' which means that whenever `subset` contains a symbol which does not refer
#' to a column, the `subset` expression is evaluated over the entire `prt`
#' object instead of over individual partitions.
#'
#' @name nse
#'
#' @export
subset.prt <- function(x, subset, select, part_safe = FALSE, drop = FALSE,
                       ...) {

  if (!isFALSE(drop)) warning("Ignoring `drop` argument.")
  if (...length() > 0L) warning("Ignoring `...` arguments.")

  assert_that(is.flag(part_safe))

  if (missing(subset)) subset <- NULL
  else subset <- rlang::enquo(subset)

  if (missing(select)) select <- NULL
  else select <- rlang::enquo(select)

  subset_quo(x, subset, select, part_safe)
}

#' @rdname nse
#'
#' @export
#'
subset_quo <- function(x, subset = NULL, select = NULL, part_safe = FALSE) {

  assert_that(is_prt(x))

  cols <- as.list(seq_along(x))
  names(cols) <- colnames(x)
  cols <- rlang::eval_tidy(select, cols)

  if (!is.null(cols)) cols <- vec_as_col_index(cols, colnames(x))

  if (is.null(subset)) {

    prt_read(x, rows = NULL, columns = cols)

  } else {

    need_cols <- match(all.vars(subset), colnames(x))

    if (all(is.na(need_cols))) {

      if (part_safe) {
        message("`part_safe` has no effect as `subset` has to be ",
                "evaluated over the entire `prt` at once.")
      }

      rows <- eval_rows(subset, nrow(x))

      prt_read(x, rows = rows, columns = cols)

    } else if (part_safe || !any(is.na(need_cols))) {

      res <- prt_lapply(x, subset_fst, cols = need_cols[!is.na(need_cols)],
                        i = subset, j = cols)
      data.table::rbindlist(res)

    } else {

      subset_prt(x, cols = need_cols[!is.na(need_cols)], i = subset, j = cols)
    }
  }
}

eval_rows <- function(quo, n_row, dat = NULL) {

  rows <- rlang::eval_tidy(quo, dat)

  vec_as_row_index(rows, n_row)
}

subset_prt <- function(x, cols, i = NULL, j = NULL) {

  if (n_part(x) > 1L) {
    message("Evaluating row subsetting over the entire `prt` at once. If ",
            "applicable consider the `part_safe` argument.")
  }

  tmp <- prt_read(x, rows = NULL, columns = cols)

  rows <- eval_rows(i, nrow(x), tmp)

  prt_read(x, rows = rows, columns = j)
}

subset_fst <- function(x, cols, i = NULL, j = NULL) {

  tmp <- fst_read(x, rows = NULL, columns = cols)

  rows <- eval_rows(i, as.integer(nrow(x)), tmp)

  fst_read(x, rows = rows, columns = j)
}
