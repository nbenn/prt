
#' NSE subsetting operations
#'
#' @inheritParams base::subset
#'
#' @param part_safe Logical flag indicating whether the `subset` expression
#' can be safely be applied to individual partitions.
#'
#' @name nse
#'
#' @export
subset.prt <- function(x, subset, select, part_safe = FALSE, drop = FALSE,
                       ...) {

  if (!isFALSE(drop)) warning("Ignoring `drop` argument.")
  if (...length() > 0L) warning("Ignoring `...` arguments.")

  if (missing(subset)) {

    subset <- NULL

  } else {

    subset <- rlang::enquo(subset)

    if (rlang::quo_is_missing(subset) || rlang::quo_is_null(subset)) {
      subset <- NULL
    }
  }

  if (missing(select)) {

    select <- NULL

  } else {

    select <- rlang::enquo(select)

    if (rlang::quo_is_missing(select) || rlang::quo_is_null(select)) {
      select <- NULL
    }
  }

  subset_quo(x, subset, select, part_safe)
}

#' @param env The environment in which `subset` and `select` are evaluated in.
#' This environment is not applicable for quosures because they have their own
#' environments.
#'
#' @rdname nse
#'
#' @export
#'
subset_quo <- function(x, subset = NULL, select = NULL, part_safe = FALSE,
                       env = parent.frame()) {

  assert_that(is_prt(x), is.flag(part_safe))

  cols <- as.list(seq_along(x))
  names(cols) <- colnames(x)
  cols <- rlang::eval_tidy(select, cols, env = env)

  if (!is.null(cols)) {
    cols <- vec_as_col_index(cols, colnames(x))
  }

  if (is.null(subset)) {

    prt_read(x, rows = NULL, columns = cols)

  } else {

    need_cols <- match(all.vars(subset), colnames(x))

    if (all(is.na(need_cols))) {

      if (part_safe) {
        message("`part_safe` has no effect as `subset` has to be ",
                "evaluated over the entire `prt` at once.")
      }

      rows <- eval_rows(subset, nrow(x), env)

      prt_read(x, rows = rows, columns = cols)

    } else if (part_safe) {

      res <- prt_lapply(x, subset_fst, cols = need_cols[!is.na(need_cols)],
                        env = env, i = subset, j = cols)
      data.table::rbindlist(res)

    } else {

      subset_prt(x, need_cols[!is.na(need_cols)], env, i = subset, j = cols)
    }
  }
}

eval_rows <- function(quo, n_row, env, dat = NULL) {

  rows <- rlang::eval_tidy(quo, dat, env = env)

  assert_that(is.logical(rows))

  vec_as_row_index(rows, n_row)
}

subset_prt <- function(x, cols, env, i = NULL, j = NULL) {

  if (n_part(x) > 1L) {
    message("Evaluating row subsetting over the entire `prt` at once. If ",
            "applicable consider the `part_safe` argument.")
  }

  tmp <- prt_read(x, rows = NULL, columns = cols)

  rows <- eval_rows(i, nrow(x), env, tmp)

  prt_read(x, rows = rows, columns = j)
}

subset_fst <- function(x, cols, env, i = NULL, j = NULL) {

  tmp <- fst_read(x, rows = NULL, columns = cols)

  rows <- eval_rows(i, as.integer(nrow(x)), env, tmp)

  fst_read(x, rows = rows, columns = j)
}
