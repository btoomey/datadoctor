#' Determine which numeric fields might actually be integers
#'
#' @param df a data frame
#' @param numeric.cols a vector containing the names of the numeric varaibles
#'  of interest
#' @export
cols_numeric_is_integer <- function(df, numeric.cols) {
  if (length(numeric.cols) == 0){
    return(character(0))
  }
  this_data <- df[, numeric.cols, drop = FALSE]
  int_test <- sapply(this_data, function(x) all(floor(x) == x))
  numeric.cols[int_test]
}
