#' Change integer type of columns to factor type.
#'
#' @param df The data.frame
#' @param int.factor.cols A vector of the names of the integer columns to be
#' converted to factors
#' @return The modified data.frame.
#' @export
change_int_to_factor <- function(df, int.factor.cols) {
  for (i in int.factor.cols) {
    eval(parse(text = paste0("df$", i, " <- as.factor(df$", i, ")")))
  }
  return(df)
}

#' Change factor type columns to numeric.
#'
#' @param df The data.frame
#' @param fact.cols A vector of the names of the integer columns to be
#' converted to factors
#' @return The modified data.frame.
#' @export
change_factor_to_number <- function (df, fact.cols) {
  df[fact.cols] <- as.data.frame(apply(df[fact.cols], 2, strip_helper))
  df
}
