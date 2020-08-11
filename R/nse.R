
#' NSE subsetting
#'
#' A cornerstone feature of `prt` is the ability to load a (small) subset of
#' rows (or columns) from a much larger tabular dataset. In order to specify
#' such a subset, an implementation of the base R S3 generic function
#' `subset()` is provided, driving the non-standard evaluation (NSE) of an
#' expression within the context of the data (with similar semantics as the
#' base R implementation for `data.frame`s).
#'
#' The functions powering NSE are `rlang::enquo()` which quote the `subset` and
#' `select` arguments and `rlang::eval_tidy()` which evaluates the
#' expressions. This allows for some
#' [`rlang`](https://rlang.r-lib.org)-specific features to be used, such as the
#' `.data`/`.env` pronouns, or the double-curly brace forwarding operator. For
#' some example code, please refer to
#' [`vignette("prt", package = "prt")`](../doc/prt.html).
#'
#' While the function `subset()` quotes the arguments passed as `subset` and
#' `select`, the function `subset_quo()` can be used to operate on already
#' quoted expressions. A final noteworthy departure from the base R interface
#' is the `part_safe` argument: this logical flag indicates whether it is safe
#' to evaluate the expression on partitions individually or whether
#' dependencies between partitions prevent this from yielding correct results.
#' As it is not straightforward to determine if dependencies might exists from
#' the expression alone, the default is `FALSE`, which in many cases will
#' result in a less efficient resolution of the row-selection and it is up to
#' the user to enable this optimization.
#'
#' @examples
#' dat <- as_prt(mtcars, n_chunks = 2L)
#'
#' subset(dat, cyl == 6)
#' subset(dat, cyl == 6 & hp > 110)
#'
#' colnames(subset(dat, select = mpg:hp))
#' colnames(subset(dat, select = -c(vs, am)))
#'
#' sub_6 <- subset(dat, cyl == 6)
#'
#' thresh <- 6
#' identical(subset(dat, cyl == thresh), sub_6)
#' identical(subset(dat, cyl == .env$thresh), sub_6)
#'
#' cyl <- 6
#' identical(subset(dat, cyl == cyl), data.table::as.data.table(dat))
#' identical(subset(dat, cyl == !!cyl), sub_6)
#' identical(subset(dat, .data$cyl == .env$cyl), sub_6)
#'
#' expr <- quote(cyl == 6)
#' # passing a quoted expression to subset() will yield an error
#' \dontrun{
#'   subset(dat, expr)
#' }
#' identical(subset_quo(dat, expr), sub_6)
#'
#' identical(
#'   subset(dat, qsec > mean(qsec), part_safe = TRUE),
#'   subset(dat, qsec > mean(qsec), part_safe = FALSE)
#' )
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

  if (!isFALSE(drop)) {
    warn_arg("drop")
  }

  if (...length() > 0L) {
    warn_arg("...")
  }

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
        inform(
          paste("`part_safe` has no effect as `subset` has to be",
                "evaluated over the entire `prt` at once."),
          "msg_ignore_part_safe"
        )
      }

      rows <- eval_rows(subset, nrow(x), env)

      prt_read(x, rows = rows, columns = cols)

    } else if (part_safe) {

      res <- prt_lapply(x, subset_fst, cols = need_cols[!is.na(need_cols)],
                        env = env, i = subset, j = cols)
      data.table::rbindlist(res)

    } else {

      if (n_part(x) > 1L && missing(part_safe)) {
        inform(
          paste("Evaluating row subsetting over the entire `prt` at once. If",
                "applicable consider the `part_safe` argument."),
          "msg_full_eval",
          .frequency = "regularly", .frequency_id = "full_eval"
        )
      }

      subset_prt(x, need_cols[!is.na(need_cols)], env, i = subset, j = cols)
    }
  }
}

eval_rows <- function(quo, n_row, env, dat = NULL) {

  rows <- rlang::eval_tidy(quo, dat, env = env)

  if (!is.logical(rows)) {
    abort(paste0("Expecting a length ", n_row, " logical vector to define a ",
                 "row subsetting."), "err_lgl_subset")
  }

  assert_that(is.logical(rows))

  vec_as_row_index(rows, n_row)
}

subset_prt <- function(x, cols, env, i = NULL, j = NULL) {

  tmp <- prt_read(x, rows = NULL, columns = cols)

  rows <- eval_rows(i, nrow(x), env, tmp)

  prt_read(x, rows = rows, columns = j)
}

subset_fst <- function(x, cols, env, i = NULL, j = NULL) {

  tmp <- fst_read(x, rows = NULL, columns = cols)

  rows <- eval_rows(i, as.integer(nrow(x)), env, tmp)

  fst_read(x, rows = rows, columns = j)
}
