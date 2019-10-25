
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

prt_lapply <- function(x, ...) lapply(unclass(x), ...)

prt_vapply <- function(x, ...) vapply(unclass(x), ...)

prt_files <- function(x) {
  get_file_name <- function(y) .subset2(y, "meta")[["path"]]
  prt_vapply(x, get_file_name, character(1L))
}
