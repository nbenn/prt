
#' Get a glimpse of your data
#'
#' The `tibble` S3 generic function [pillar::glimpse()] is implemented for
#' `prt` objects as well. Inspired by the output of [str()] when applied to
#' `data.frames`, this function is intended to display the structure of the
#' data in terms of columns, irrespective of how the data is organized in terms
#' of `R` objects. Similarly to [trunc_dt()], the function providing the bulk
#' of functionality, `glimpse_dt()`, is exported such that implementing a
#' class specific [pillar::glimpse()] function for other classes that
#' representing tabular data is straightforward.
#'
#' Alongside a `prt`-specific [pillar::glimpse()] method, a [str()] method is
#' provided as well for `prt` objects. However, breaking with base `R`
#' expectations, it is not the structure of the object in terms of `R` objects
#' that is shown, but in the same spirit as [pillar::glimpse()] it is the
#' structure of the data that is printed. How this data is represents with
#' respect to `R` objects is abstracted away as to show output as would be
#' expected if the data were represented by a `data.frame`.
#'
#' In similar spirit as [trunc_dt()] and `glimpse_dt()`, a `str_dt()` function
#' is exported which provides the core functionality driving the `prt`
#' implementation of [str()]. This function requires availability of a
#' [head()] function for any object that is passed and output can be
#' customized by implementing an optional `str_sum()` function.
#'
#' @examples
#' cars <- as_prt(mtcars)
#'
#' pillar::glimpse(cars)
#' pillar::glimpse(cars, width = 30)
#'
#' str(cars)
#' str(cars, vec.len = 1)
#'
#' str(unclass(cars))
#'
#' str_sum(cars)
#'
#' @inheritParams pillar::glimpse
#'
#' @rdname glimpse
#'
#' @importFrom tibble glimpse
#' @importFrom pillar new_pillar_title new_pillar_type
#'
#' @export
#'
glimpse.prt <- function(x, width = NULL, ...) {
  glimpse_dt(x = x, width = width)
  invisible(x)
}

#' @rdname glimpse
#'
#' @export
#'
glimpse_dt <- function(x, width = NULL) {

  width <- print_width(width, allow_inf = FALSE)

  if (!is.finite(width)) {
    abort("`glimpse()` requires a finite value for the `width` argument.",
          "err_glimp_inf_width")
  }

  cat_line("Rows: ", big_mark(nrow(x)))

  rows <- as.integer(width / 3)
  df <- as.data.frame(head(x, rows))
  cat_line("Columns: ", big_mark(ncol(df)))

  summary <- tbl_sum(x)
  brief_summary <- summary[-1]

  if (length(brief_summary) > 0L) {
    cat_line(names(brief_summary), ": ", brief_summary)
  }

  if (ncol(df) == 0) return(invisible(x))

  var_types <- vapply(lapply(df, new_pillar_type), format, character(1L))
  ticked_names <- format(new_pillar_title(tick_if_needed(names(df))))
  var_names <- paste0("$ ", justify(ticked_names, right = FALSE), " ",
                      var_types, " ")

  data_width <- width - crayon::col_nchar(var_names) - 2
  formatted <- vapply(df, function(x) collapse(format_row(x)), character(1L))
  truncated <- str_trunc(formatted, data_width)

  if (!crayon::has_color()) {
    var_names <- crayon::strip_style(var_names)
  }

  cat_line(var_names, truncated)

  invisible(NULL)
}

format_row <- function(x) UseMethod("format_row")

#' @export
format_row.default <- function(x) {
  dims <- dim(x)

  if (!is.null(dims)){
    dims_out <- paste0(dims, collapse = " x ")
    out <- paste0("<", class(x)[1], "[", dims_out, "]>")
    out
  } else {
    format(x, trim = TRUE, justify = "none")
  }
}

#' @export
format_row.character <- function(x) encodeString(x, quote = '"')

#' @export
format_row.factor <- function(x) {
  if (any(grepl(",", x, fixed = TRUE))) {
    encodeString(as.character(x), quote = '"')
  } else {
    format(x, trim = TRUE, justify = "none")
  }
}

#' @rdname glimpse
#'
#' @export
#'
str_sum <- function(x) UseMethod("str_sum")

#' @export
str_sum.prt <- function(x) {

  ncol <- ncol(x)
  cls <- paste0("'", class(x), "'", collapse = ", ")
  npart <- n_part(x)

  paste0(
    cls, ":\t", nrow(x), " obs. of ", ncol, " variable", if (ncol != 1) "s",
    " in ", npart, " partition", if (npart != 1) "s", if (ncol > 0) ":", "\n"
  )
}

#' @export
str_sum.data.frame <- function(x) {

  ncol <- ncol(x)
  cls <- paste0("'", class(x), "'", collapse = ", ")

  paste0(
    cls, ":\t", nrow(x), " obs. of ", ncol, " variable", if (ncol != 1) "s\n"
  )
}

#' @inheritParams utils::str
#'
#' @rdname glimpse
#'
#' @importFrom utils str
#'
#' @export
#'
str.prt <- function(object, ...) {
  invisible(str_dt(object, ...))
}

#' @rdname glimpse
#'
#' @importFrom utils capture.output
#'
#' @export
#'
str_dt <- function(x, ...) {

  dots <- list(...)

  if ("vec.len" %in% names(dots)) {
    len <- dots[["vec.len"]]
  } else {
    len <- utils::strOptions()$vec.len
  }

  dat <- head(x, len * 3L + 1L)

  if (!"give.length" %in% names(dots)) {
    dots[["give.length"]] <- FALSE
  }

  dots[["no.list"]] <- TRUE

  cat(str_sum(x))

  if (isTRUE(dots[["give.length"]]) && nrow(dat) > 0L) {
    res <- capture.output(do.call("str", c(list(c(dat)), dots)))
    res <- sub(paste0("\\[1:", nrow(dat), "\\]"),
               paste0(  "[1:", nrow(x),     "]"), res)
    cat_line(res)
  } else {
    do.call("str", c(list(c(dat)), dots))
  }

  invisible()
}
