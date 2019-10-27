
split_indices <- function(len, n_chunks) {

  assert_that(is.count(len), is.count(n_chunks))

  if (len == 1L || n_chunks == 1L) {

    rep.int(1L, len)

  } else {

    i <- seq_len(len)

    fuzz <- min((len - 1L) / 1000, 0.4 * len / n_chunks)
    breaks <- seq(1 - fuzz, len + fuzz, length.out = n_chunks + 1L)
    bins <- cut(i, breaks)

    as.integer(bins)
  }
}

new_names <- function(old_names = character(0L), n = 1L,
                      chars = c(letters, LETTERS, 0L:9L), length = 15L) {

  repeat{
    res <- replicate(n, paste(sample(chars, length), collapse = ""))
    if (length(res) == length(unique(res)) && !any(res %in% old_names)) break
  }

  res
}

prt_lapply <- function(x, ...) lapply(unclass(x), ...)

prt_vapply <- function(x, ...) vapply(unclass(x), ...)

prt_files <- function(x) {
  prt_vapply(x, fst_filename, character(1L))
}

fst_filename <- function(x) {
  assert_that(inherits(x, "fst_table"))
  .subset2(x, "meta")[["path"]]
}

prt_nrows <- function(x) prt_vapply(x, nrow, numeric(1L))

num_ind_to_part_no <- function(ind, lengths) {

  assert_that(all(ind > 0L, na.rm = TRUE))

  if (length(lengths) == 1L) return(rep.int(1L, length(ind)))

  res <- as.integer(cut(ind, c(0L, cumsum(lengths))))

  if (anyNA(res)) {
    non_na <- !is.na(res)
    res <- rep(c(NA, res[non_na]),
               diff(c(1L, which(non_na), length(res) + 1L)))
  }

  res
}

num_ind_to_part_ind <- function(ind, lengths, part_no = NULL) {

  if (is.null(part_no)) part_no <- num_ind_to_part_no(ind, lengths)

  assert_that(length(ind) == length(part_no))

  as.integer(ind - c(0, cumsum(lengths)[-length(lengths)])[part_no])
}

prt_read <- function(x, rows = NULL, columns = NULL) {

  if (is.numeric(columns)) columns <- colnames(x)[columns]

  if (is.null(rows)) {

    res <- lapply(prt_files(x), fst::read_fst, columns = columns,
                  as.data.table = TRUE)
    res <- data.table::rbindlist(res)

  } else {

    nrows <- prt_nrows(x)
    parts <- num_ind_to_part_no(rows, nrows)
    inds <- num_ind_to_part_ind(rows, nrows, parts)

    res <- Map(fst_read, unclass(x),
               split(inds, factor(parts, levels = seq_along(nrows))),
               MoreArgs = list(columns = columns))
    res <- data.table::rbindlist(res)

    if (is.unsorted(parts)) {

      tmp <- new_names(colnames(res))
      data.table::set(res, j = tmp, value = order(parts))
      on.exit(data.table::set(res, j = tmp, value = NULL))

      res <- data.table::setorderv(res, tmp)
    }
  }

  res
}

fst_read <- function(x, rows = NULL, columns = NULL) {

  if (!is.null(columns)) {
    # unfortunately, data.table is not capable of representing zero column
    # non-zero row tables, unlike tibble and data.frame
    if (length(columns) == 0) return(data.table::data.table())
    else assert_that(is.character(columns))
  }

  if (is.null(rows)) {
    from <- 1L
    to <- NULL
  } else if (length(rows) == 0) {
    from <- 1L
    to <- 1L
  } else {
    from <- min(rows, na.rm = TRUE)
    to <- min(max(rows, na.rm = TRUE), nrow(x))
  }

  res <- fst::read_fst(fst_filename(x), columns = unique(columns), from = from,
                       to = to, as.data.table = TRUE)

  if (!is.null(columns)) {

    res_cols <- colnames(res)

    if (!identical(columns, res_cols)) {

      if (setequal(columns, res_cols)) data.table::setcolorder(res, columns)
      else res <- res[, columns, with = FALSE]
    }
  }

  if (!is.null(rows) && length(rows) == 0) {
    res[-1L, ]
  } else if (length(rows) > 0L && (anyNA(rows) || any(diff(rows) != 1L))) {
    # if we could remove rows by reference, reordering & subsetting could be
    # done without creating a copy
    res[1L + rows - from, ]
  } else {
    res
  }
}

dots_n <- function(...) {
  nargs()
}

isFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}

print_width <- function(width = NULL, allow_inf = TRUE) {

  if (!is.null(width)) return(width)

  width <- getOption("tibble.width")

  if (!is.null(width) && !allow_inf && is.finite(width)) width
  else getOption("width")
}

cat_line <- function(...) {
  cat(paste0(..., "\n"), sep = "")
}

big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, format = "d", ...)
}

#' @inheritParams tibble::tbl_sum
#'
#' @rdname glimpse
#'
#' @importFrom tibble tbl_sum
#'
#' @export
#'
tbl_sum.prt <- function(x) {
  c("A prt (partitioned fst)" = dim_desc(x),
    "Partitioning" = part_desc(x))
}

dim_desc <- function(x) {
  paste0(vapply(dim(x), big_mark, character(1L)),
         collapse = spaces_around(times()))
}

part_desc <- function(x) {
  paste0(length(x), " parts [",
         paste(prt_nrows(x), collapse = ", "), "] rows")
}

times <- function(fancy = l10n_info()$`UTF-8`) {
  if (fancy) cli::symbol$times
  else "x"
}

spaces_around <- function(x) {
  paste0(" ", x, " ")
}

tick <- function(x) {
  ifelse(is.na(x), "NA", encodeString(x, quote = "`"))
}

tick_if_needed <- function(x) {
  needs_ticks <- !is_syntactic(x)
  x[needs_ticks] <- tick(x[needs_ticks])
  x
}

is_syntactic <- function(x) {
  ret <- make.names(x) == x
  ret[is.na(x)] <- FALSE
  ret
}

nchar_width <- function(x) {
  nchar(x, type = "width")
}

justify <- function(x, right = TRUE, space = " ") {
  if (length(x) == 0L) return(character())
  width <- nchar_width(x)
  max_width <- max(width)
  spaces_template <- paste(rep(space, max_width), collapse = "")
  spaces <- vapply(max_width - width, substr, character(1L),
                   x = spaces_template, start = 1L)
  if (right) {
    paste0(spaces, x)
  } else {
    paste0(x, spaces)
  }
}

collapse <- function(x) paste(x, collapse = ", ")

str_trunc <- function(x, max_width) {

  width <- nchar(x)

  nchar_ellipsis <- nchar_width(cli::symbol$ellipsis)

  for (i in seq_along(x)) {
    if (width[i] <= max_width[i]) next

    x[i] <- paste0(substr(x[i], 1, max_width[i] - nchar_ellipsis),
                   cli::symbol$ellipsis)
  }

  x
}
