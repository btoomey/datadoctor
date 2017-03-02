#' Convert any blank values to missing (NA) values
#'
#' @param df The data.frame
#' @param cols.to.use The columns for which blanks are to be convert to missing
#' @return The modified data.frame.

blanks_to_missing <- function(df, cols.to.use = names(df)) {
  # The function to use with apply
  the_func <- function(x) {
    if (class(x) == "factor") {
      x[as.character(x) == ""] <- NA
    }
    x
  }
  as.data.frame(lapply(df[, cols.to.use, drop = FALSE], the_func))
}

#' Report on the number and percentage of missing (NA) values for a data.frame
#'
#' @param df The data.frame
#' @return A data.frame containing the missing value report.
#' @export
cols_missing_blank <- function(df) {
  # Address possible blanks in the data
  df <- blanks_to_missing(df)
  # Calculate the measures
  na_num <- sapply(df, function(x) sum(as.numeric(is.na(x))))
  na_pct <- 100*round(na_num/nrow(df), 4)
  # Organize and report the output
  data.frame(Column = names(df), Number_Missing_Blank = na_num, Percent_Missing_Blank = na_pct)
}
