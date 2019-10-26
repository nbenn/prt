
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
  get_file_name <- function(y) .subset2(y, "meta")[["path"]]
  prt_vapply(x, get_file_name, character(1L))
}

prt_nrows <- function(x) prt_vapply(x, nrow, numeric(1L))

num_ind_to_part_no <- function(ind, lengths) {

  if (length(lengths) == 1L) return(rep.int(1L, length(ind)))

  res <- as.integer(cut(ind, c(0L, cumsum(lengths))))

  assert_that(!anyNA(res))

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

    res <- Map(fst_read, prt_files(x),
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

fst_read <- function(file, rows = NULL, columns = NULL) {

  if (!is.null(columns)) {
    assert_that(is.character(columns), length(columns) > 0L)
  }

  if (is.null(rows)) {
    rng <- list(1L, NULL)
  } else if (length(rows) == 0) {
    rng <- list(1L, 1L)
  } else {
    rng <- as.list(range(rows))
  }

  res <- fst::read_fst(file, columns = columns, from = rng[[1L]],
                        to = rng[[2L]], as.data.table = TRUE)

  if (is.null(rows)) {
    res
  } else {
    res[1L + rows - rng[[1L]], ]
  }
}
