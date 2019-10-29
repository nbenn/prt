
#' @importFrom assertthat assert_that is.count is.string is.flag
NULL

.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, c("...length", "isFALSE", "strrep", "trimws"))
}
