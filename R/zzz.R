
#' @importFrom assertthat assert_that is.count is.string is.flag
#' @importFrom rlang abort warn inform
NULL

.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, c("...length", "isFALSE", "strrep", "trimws"))
}
