
#' @importFrom assertthat assert_that
NULL

#' Constructor for prt objects
#'
#' @param files Character vector of file name(s).
#'
#' @export
new_prt <- function(files) {

  assert_that(
    all(file.exists(files)), length(file) > 1L
  )

  structure(lapply(files, fst::fst), class = "prt")
}
