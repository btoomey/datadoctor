blanksToMissing <- function(df, cols.to.use = names(df)) {
  # The function to use with apply
  the_func <- function(x) {
    if (class(x) == "factor") {
      x[as.character(x) == ""] <- NA
    }
    x
  }
  as.data.frame(lapply(df[, cols.to.use, drop = FALSE], the_func))
}

#' @export
cols_missing_blank <- function(df) {
  # Address possible blanks in the data
  df <- blanksToMissing(df)
  # Calculate the measures
  na_num <- sapply(df, function(x) sum(as.numeric(is.na(x))))
  na_pct <- 100*round(na_num/nrow(df), 4)
  # Organize and report the output
  data.frame(Column = names(df), Number_Missing_Blank = na_num, Percent_Missing_Blank = na_pct)
}
