#' Entropy measure
#'
#' \code{entropy} calculates the Shannon entropy measure via maximum likelihood
#' for a numeric or integer variable. The user may specify the base,
#' \code{exp(1)} for the natrual base, \code{2} for bits, and so on.
#'
#' @param x numeric or integer vector
#' @param base of the logarithm, default is exp(1), the natural logarithm
entropy <- function(x, base = exp(1)) {
   if (!is.factor(x)) {
    stop("The provided value of x is not factor")
  }
  percentages <- table(x)/length(x)
  -sum(percentages*posLog(percentages, base = base))
}
