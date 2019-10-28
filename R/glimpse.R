
#' Get a glimpse of your data
#'
#' @inheritParams tibble::glimpse
#'
#' @rdname glimpse
#'
#' @importFrom tibble glimpse
#' @importFrom pillar new_pillar_title new_pillar_type
#'
#' @export
#'
glimpse.prt <- function(x, width = NULL, ...) {
  gplimpse_prt(x = x, width = width)
  invisible(x)
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

  ncol <- ncol(object)
  npart <- length(object)

  cat("'prt':\t", nrow(object), " obs. of ", ncol, " variable",
      if (ncol != 1) "s",  " in ", npart, " partition",
      if (npart != 1) "s", if (ncol > 0) ":", "\n", sep = "")

  dots <- list(...)

  if (length(dots) && any("vec.len" == names(dots))) {
    len <- dots[["vec.len"]]
  } else {
    len <- utils::strOptions()$vec.len
  }

  if (length(dots)) {
    if (any("give.length" == names(dots))) {
      warning("Ignoring `give.length` argument.")
      dots$give.length <- NULL
    }
    if (any("no.list" == names(dots))) {
      dots$no.list <- NULL
    }
  }

  dat <- head(object, len * 3L + 1L)

  args <- c(
    list(c(dat)),
    dots,
    list(no.list = TRUE, give.length = FALSE)
  )

  invisible(do.call("str", args))
}

gplimpse_prt <- function(x, width = NULL) {

  width <- print_width(width, allow_inf = FALSE)

  if (!is.finite(width)) {
    stop("`glimpse()` requires a finite value for the `width` argument.")
  }

  cat_line("Rows: ", big_mark(nrow(x)))

  rows <- as.integer(width / 3)
  df <- as.data.frame(head(x, rows))
  cat_line("Columns: ", big_mark(ncol(df)))

  summary <- tibble::tbl_sum(x)
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
