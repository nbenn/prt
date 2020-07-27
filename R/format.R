
#' Printing prt
#'
#' @inheritParams tibble::print.tbl
#'
#' @rdname formatting
#'
#' @export
#'
print.prt <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
  invisible(x)
}

#' @inheritParams tibble::format.tbl
#'
#' @rdname formatting
#'
#' @export
#'
format.prt <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  mat <- trunc_dt(x, n = n, width = width, n_extra = n_extra)
  format(mat)
}

#' @export
print.trunc_dt <- function(x, ...) {
  cat_line(format(x, ...))
  invisible(x)
}

#' @inheritParams print.prt
#'
#' @keywords internal
#'
#' @rdname internal
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

  rowid <- big_mark(rowid)

  shrunk <- shrink_dt(df, rows)

  if (shrunk$rows_missing > 0L) {
    rowid <- add_in_between(rowid, n, ellipsis())
  }

  trunc_info <- list(
    width = width, rows_total = rows, rows_min = nrow(df),
    n_extra = n_extra, summary = tibble::tbl_sum(x), row_id = rowid
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

#' knit_print method for trunc dt
#'
#' @keywords internal
#'
#' @export
#'
knit_print.trunc_dt <- function(x, options) {

  header <- format_header(x)
  summary <- paste0(names(header), ": ", header)

  squeezed <- squeeze_dt(x, width = x$width)

  kable <- knitr::knit_print(squeezed)
  extra <- format_footer(x, squeezed)

  if (length(extra) > 0) {
    extra <- wrap("(", collapse(extra), ")", width = x$width)
  } else {
    extra <- "\n"
  }

  res <- paste(c("", "", summary, "", kable, "", extra), collapse = "\n")
  knitr::asis_output(crayon::strip_style(res), cacheable = TRUE)
}


shrink_dt <- function(df, rows) {

  n <- nrow(df)

  needs_dots <- (rows > n)

  if (needs_dots) {
    rows_missing <- rows - n
  } else {
    rows_missing <- 0L
  }

  list(
    mcf = pillar::colonnade(df, has_row_id = FALSE, needs_dots = needs_dots),
    rows_missing = rows_missing
  )
}

add_empty_row <- function(x) {
  if (length(x) == 0L) return(x)
  n <- length(x[[1L]][[1L]][["shaft_format"]]) / 2L
  lapply(x, function(y) {
    lapply(y, function(z) {
      z[["shaft_format"]] <- add_in_between(z[["shaft_format"]], n, " ")
      z
    })
  })
}

add_row_id <- function(x, rowid) {
  if (length(x) == 0L) return(x)
  row_width <- max(crayon::col_nchar(rowid))
  lapply(x, function(y) {
    c(list(list(capital_format = rep(strrep(" ", row_width), 2L),
                shaft_format = format(rowid))), y)
  })
}

squeeze_dt <- function(x, width) {

  term_width <- getOption("width")
  id_width <- max(nchar(x$row_id))

  on.exit(options(width = term_width))
  options(width = term_width - id_width - 1L)

  res <- pillar::squeeze(x$mcf, width = width - id_width - 1L)

  attribs <- attributes(res)

  if (x$rows_missing > 0L) res <- add_empty_row(res)
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
    extra_cols <- c(extra_cols[!is.na(extra_cols)], ellipsis())
  }

  paste0(": ", collapse(extra_cols))
}

pre_dots <- function(x) {
  if (length(x) > 0) paste0(ellipsis(), " ", x)
  else character()
}