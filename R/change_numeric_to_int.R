#' Change a numeric column to integer, if it appears to be an integer
#' @param df The data frame
#' @param numeric.int.cols Columns that are numeric but shoud be integers
#' @return Modified data.frame
#' @export
change_numeric_to_int <- function(df, numeric.int.cols) {
  for (i in numeric.int.cols) {
    eval(parse(text = paste0("df$", i, " <- as.integer(df$", i, ")")))
  }
  df
}
