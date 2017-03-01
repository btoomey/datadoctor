#' Determine which numeric variables in a data frame are actually integers
#'
#' @param df a data frame or a matrix
#' @param numeric.cols a character vector containing the names of numeric variables
cols_integer_is_numeric <- function(df, numeric.cols) {
  the_func <- function(x, df) {
    check <- df[,x] == floor(df[,x])
    out <- if (check) {
      x
    } else {
      NA
    }
    out
  }
  out_vec <- sapply(numeric.cols, the_fun, df = df)
  out_vec[!is.na(out_vec)]
}