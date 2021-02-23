
#' Printing prt
#'
#' Printing of `prt` objects combines the concise yet informative design
#' of only showing as many columns as the terminal width allows for, introduced
#' by `tibble`, with the `data.table` approach of showing both the first and
#' last few rows of a table. Implementation wise, the interface is designed to
#' mimic that of `tibble` printing as closely as possibly, offering the same
#' function arguments and using the same option settings (and default values)
#' as introduced by `tibble`.
#'
#' While the function [tibble::trunc_mat()] does most of the heavy lifting
#' for formatting `tibble` printing output, `prt` exports the function
#' `trunc_dt()`, which drives analogous functionality while adding the
#' top/bottom `n` row concept. This function can be used for creating [print()]
#' methods for other classes which represent tabular data, given that this
#' class implements [dim()], [head()] and [tail()] (and optionally
#' [tibble::tbl_sum()]) methods. For an example of this, see
#' [`vignette("prt", package = "prt")`](../doc/prt.html).
#'
#' The following session options are set by `tibble` and are respected by
#' `prt`, as well as any other package that were to call `trunc_dt()`:
#'
#' * `tibble.print_max`: Row number threshold: Maximum number of rows printed.
#'   Set to `Inf` to always print all rows.  Default: 20.
#' * `tibble.print_min`: Number of rows printed if row number threshold is
#'   exceeded. Default: 10.
#' * `tibble.width`: Output width. Default: `NULL` (use `width` option).
#' * `tibble.max_extra_cols`: Number of extra columns printed in reduced form.
#'   Default: 100.
#'
#' Both `tibble` and `prt` rely on `pillar` for formatting columns and
#' therefore, the following options set by `pillar` are applicable to `prt`
#' printing as well.
#'
#' @inheritSection pillar::`pillar-package` Package options
#'
#' @examples
#' cars <- as_prt(mtcars)
#'
#' print(cars)
#' print(cars, n = 2)
#' print(cars, width = 30)
#' print(cars, width = 30, n_extra = 2)
#'
#' @inheritParams tibble::print.tbl
#'
#' @importFrom utils packageVersion
#' @importFrom tibble tbl_sum
#'
#' @rdname formatting
#'
#' @export
#'
print.prt <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
  invisible(x)
}

#' @rdname formatting
#'
#' @export
#'
format.prt <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  format(trunc_dt(x, n = n, width = width, n_extra = n_extra))
}

#' @export
print.trunc_dt <- function(x, ...) {
  cat_line(format(x, ...))
  invisible(x)
}

#' @rdname formatting
#'
#' @export
#'
trunc_dt <- function(x, n = NULL, width = NULL, n_extra = NULL) {

  rows <- nrow(x)

  if (is.null(n) || n < 0) {
    if (rows > get_opt("print_max")) {
      n <- get_opt("print_min")
    } else {
      n <- rows
    }
  }

  if (is.null(n_extra)) n_extra <- get_opt("max_extra_cols")

  if (nrow(x) < 2 * n) {
    df <- head(x, nrow(x))
    rowid <- seq_len(nrow(x))
  } else {
    df <- rbind(head(x, n), tail(x, n))
    rowid <- c(seq_len(n), seq.int(nrow(x) - n + 1L, nrow(x)))
  }

  df <- as.data.frame(df)

  rowid <- big_mark(rowid)

  shrunk <- shrink_dt(df, rows)

  if (shrunk$rows_missing > 0L) {
    rowid <- add_in_between(rowid, n, cli::symbol$ellipsis)
  }

  trunc_info <- list(
    width = width, rows_total = rows, rows_min = nrow(df),
    n_extra = n_extra, summary = tbl_sum(x), row_id = rowid
  )

  structure(
    c(shrunk, trunc_info),
    class = c(paste0("trunc_dt_", class(x)), "trunc_dt")
  )
}

#' @export
format.trunc_dt <- function(x, width = NULL, ...) {

  if (is.null(width)) {
    width <- x$width
  }

  width <- print_width(width)

  header <- format_header(x)
  header <- paste0(justify(paste0(names(header), ":"),
                           right = FALSE, space = "\u00a0"),
                   " ", header)

  comment <- format_comment(header, width = width)
  squeezed <- squeeze_dt(x, width = width)

  footer <- format_comment(pre_dots(format_footer(x, squeezed)), width = width)

  c(
    pillar::style_subtle(comment),
    format(squeezed),
    pillar::style_subtle(footer)
  )
}

shrink_dt <- function(df, rows) {

  n <- nrow(df)

  if (rows > n) {
    rows_missing <- rows - n
  } else {
    rows_missing <- 0L
  }

  list(
    mcf = pillar::colonnade(df, has_row_id = FALSE),
    rows_missing = rows_missing
  )
}

add_empty_row <- function(x) {

  if (length(x) == 0L) {
    return(x)
  }

  add_row <- function(x) {
    if (length(x) == 0L) return(x)
    mid <- (length(x) - 2L) / 2L
    res <- c(head(x, n = mid + 2L), " ", tail(x, n = mid))
    structure(res, class = class(x))
  }

  add_shaft <- function(x, n) {
    x[["shaft_format"]] <- add_in_between(x[["shaft_format"]], n, " ")
    x
  }

  if (packageVersion("pillar") < "1.5.0") {

    n <- length(x[[1L]][[1L]][["shaft_format"]]) / 2L

    lapply(x, lapply, add_shaft, n)

  } else {

    lapply(x, add_row)
  }
}

add_row_id <- function(x, rowid) {

  if (length(x) == 0L) {
    return(x)
  }

  if (packageVersion("pillar") < "1.5.0") {

    do_add <- function(x, width, id) {
      c(list(list(capital_format = rep(strrep(" ", width), 2L),
                  shaft_format = format(id))), x)
    }

  } else {

    do_add <- function(x, width, id) {
      if (length(x) == 0L) return(x)
      res <- paste(c(rep(strrep(" ", width), 2L),
                   format(id, justify = "right")), x)
      structure(res, class = class(x))
    }
  }

  lapply(x, do_add, max(crayon::col_nchar(rowid)), rowid)
}

squeeze_dt <- function(x, width) {

  term_width <- getOption("width")
  id_width <- max(nchar(x$row_id))

  on.exit(options(width = term_width))
  options(width = term_width - id_width - 1L)

  res <- pillar::squeeze(x$mcf, width = width - id_width - 1L)

  attribs <- attributes(res)

  if (x$rows_missing > 0L) {
    res <- add_empty_row(res)
  }

  res <- add_row_id(res, pillar::style_subtle(x$row_id))

  attributes(res) <- attribs

  res
}

format_header <- function(x) {
  x$summary
}

format_footer <- function(x, squeezed_colonnade) {

  extra_rows <- format_footer_rows(x)
  extra_cols <- format_footer_cols(x,
    pillar::extra_cols(squeezed_colonnade, n = x$n_extra)
  )

  extra <- c(extra_rows, extra_cols)

  if (length(extra) >= 1) {
    extra[[1]] <- paste0("with ", extra[[1]])
    extra[-1] <- vapply(extra[-1], function(ex) paste0("and ", ex),
                        character(1L))
    collapse(extra)
  } else {
    character()
  }
}

format_footer_rows <- function(x) {
  if (x$rows_missing == 0L) {
    NULL
  } else if (x$rows_missing == 1L) {
    "1 more row"
  } else {
    paste0(big_mark(x$rows_missing), " more rows")
  }
}

format_footer_cols <- function(x, extra_cols) {

  if (length(extra_cols) == 0) return(NULL)

  vars <- format_extra_vars(extra_cols)

  paste0(
    big_mark(length(extra_cols)), " ",
    if (!identical(x$rows_total, 0L) && x$rows_min > 0) "more ",
    if (length(extra_cols) == 1L) "variable" else "variables",
    vars
  )
}

format_extra_vars <- function(extra_cols) {

  if (is.na(extra_cols[1])) return("")

  if (anyNA(extra_cols)) {
    extra_cols <- c(extra_cols[!is.na(extra_cols)], cli::symbol$ellipsis)
  }

  paste0(": ", collapse(extra_cols))
}

pre_dots <- function(x) {
  if (length(x) > 0) paste0(cli::symbol$ellipsis, " ", x)
  else character()
}
