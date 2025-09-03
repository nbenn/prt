
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
#' class implements [dim()], [utils::head()] and [utils::tail()] (and
#' optionally [pillar::tbl_sum()]) methods. For an example of this, see
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
#' @inheritSection pillar::pillar_options Options for the pillar package
#'
#' @examples
#' cars <- as_prt(mtcars)
#'
#' print(cars)
#' print(cars, n = 2)
#' print(cars, width = 30)
#' print(cars, width = 30, max_extra_cols = 2)
#'
#' @inheritParams tibble::print.tbl
#'
#' @rdname formatting
#'
#' @export
#'
print.prt <- function(x, ..., n = NULL, width = NULL, max_extra_cols = NULL) {

  cat_line(
    format(x, ..., n = n, width = width, max_extra_cols = max_extra_cols)
  )

  invisible(x)
}

#' @rdname formatting
#' @export
format.prt <- function(x, ..., n = NULL, width = NULL, max_extra_cols = NULL) {
  format_dt(
    x, ..., n = n, width = width, max_extra_cols = max_extra_cols
  )
}

#' @rdname formatting
#' @export
format_dt <- function(x, ..., n = NULL, width = NULL, max_extra_cols = NULL,
                      max_footer_lines = NULL) {

  rows <- nrow(x)

  if (is.null(n) || n < 0) {
    if (rows > get_opt("print_max")) {
      n <- get_opt("print_min")
    } else {
      n <- rows
    }
  }

  if (is.null(max_extra_cols)) {
    max_extra_cols <- get_opt("max_extra_cols")
  }

  if (rows <= n * 2L) {
    df <- head(x, rows)
    rowid <- seq_len(rows)
    print_all <- TRUE
  } else {
    df <- rbind(head(x, n), tail(x, n))
    rowid <- c(seq_len(n), seq.int(rows - n + 1L, rows))
    print_all <- FALSE
  }

  rowid <- big_mark(rowid)
  rowid_width <- max(nchar(rowid))

  tbl <- as.data.frame(df)

  class(tbl) <- c(paste0(class(x), "_prnt"), "dt_prnt", "tbl", "data.frame")

  attr(tbl, "row_ids") <- rowid
  attr(tbl, "rowid_width") <- rowid_width

  setup <- pillar::tbl_format_setup(tbl, width, ..., n = n * 2L,
                                    max_extra_cols = max_extra_cols)

  setup$n_half <- n
  setup$print_all <- print_all
  setup$rowid_width <- rowid_width

  if (!print_all) {
    setup$rows_total <- rows
    setup$rows_missing <- rows - n
  }

  setup$tbl_sum <- tbl_sum(x)

  header <- pillar::tbl_format_header(tbl, setup)
  body <- tbl_format_body(tbl, setup)
  footer <- pillar::tbl_format_footer(tbl, setup)

  c(header, body, footer)
}

#' @rdname formatting
#' @export
trunc_dt <- function(...) {
  .Deprecated("format_dt")
  format_dt(...)
}

#' @importFrom pillar tbl_format_body
#' @export
tbl_format_body.dt_prnt <- function(x, setup, ...) {

  force(setup)

  res <- setup$body

  if (!setup$print_all && length(res)) {

    col_head_len <- 2L
    col_body_len <- setup$n_half * 2L

    stopifnot(length(res) %% (col_head_len + col_body_len) == 0L)

    n_tiers <- length(res) %/% (col_head_len + col_body_len)

    stopifnot(n_tiers >= 1L)

    inds <- seq_len(n_tiers) * (col_head_len + setup$n_half) +
      (seq_len(n_tiers) - 1L)

    ell <- style_hint(cli::symbol$ellipsis)

    for (ind in inds) {
      res <- c(res[seq_len(ind)], ell, res[seq.int(ind + 1L, length(res))])
    }
  }

  res
}

#' @importFrom pillar ctl_new_rowid_pillar
#' @export
ctl_new_rowid_pillar.dt_prnt <- function(controller, x, width, ...) {

  out <- NextMethod()

  rowid <- attr(controller, "row_ids")
  width <- attr(controller, "rowid_width")

  pillar::new_pillar(
    list(
      title = out$title,
      type = out$type,
      data = pillar::pillar_component(
        pillar::new_pillar_shaft(
          list(row_ids = rowid),
          width = width,
          class = "pillar_rif_shaft"
        )
      )
    ),
    width = width
  )
}

cli_grey_80 <- cli::make_ansi_style("grey80", grey = TRUE)

style_hint <- function(x) {
  if (isTRUE(getOption("pillar.subtle", default = TRUE))) {
    cli_grey_80(x)
  } else {
    x
  }
}

#' @importFrom pillar tbl_sum
#' @export
tbl_sum.prt <- function(x) {
  c("A prt" = dim_desc(x), "Partitioning" = part_desc(x))
}
