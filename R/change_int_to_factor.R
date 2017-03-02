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


#' @export
change_factor_to_number <- function (df, fact_cols) {
  df[fact_cols] <- as.data.frame(apply(df[fact_cols], 2, strip_helper))
  df
}
