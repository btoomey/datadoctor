#' Strip out newlines and white space from elements of a character vector.
#'
#' @param x a character vector to clean
trimNewlines <- function(x) {
  if (!is.character(x)) {
    stop('The argument "x" must be a character vector.')
  }
  AlteryxPredictive:::trim.blanks(sub("\\n", "", x))
}
