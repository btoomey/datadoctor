#' Determine which integer columns have sequential values
#'
#' @param df a data frame or a matrix
#' @param integer.cols a character vector containing the names of integer variables
#' @export
cols_int_sequential <- function(df, integer.cols) {
  the_func <- function(x) {
    unq_x <- unique(x)[order(unique(x))]
    all(unq_x[2:length(unq_x)] - unq_x[1:(length(unq_x) - 1)] == 1)
  }
  df2 <- df[, names(df) %in% integer.cols, drop = FALSE]
  the_test <- sapply(df2, the_func)
  out <- if (length(the_test) > 0) {
    names(df2)[the_test]
  } else {
    character(0)
  }
  out
}
