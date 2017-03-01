#' Produces logarithm values for strictly positive values, zero otherwise
#'
#' @param x numeric vector
#' @param base of the logarithm, default is exp(1), the natural logarithm
posLog <- function(x, base = exp(1)) {
  x[x <= 0] <- 1
  log(x, base = base)
}
