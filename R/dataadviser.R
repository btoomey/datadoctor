#' Convert whitespace entries to NAs
#'
#' @param df The data.frame you wish to change missing values to NAs.
#' @export
change_missing_to_na <- function(df) {
  is_factor <- sapply(df, function(x) class(x) == 'factor')
  factor_cols <- names(df)[is_factor]
  df[factor_cols] <- as.data.frame(apply(df[factor_cols], 2, function(x) gsub('^\\s*$', NA, x)))
  df
}

#' Return names of columns that has only one value
#'
#' @param df The data.frame to examine
#' @param cols Names of columns to examine, will use all columns of df, if it
#' is not specified.
#' @return A vector of columns names with only one value
#' @export
cols_with_unique_val <- function(df, cols = NULL) {
  if (is.null(cols)) {
    cols = colnames(df)
  }
  cond <- sapply(df[cols], function(x){
    length(unique(x)) == 1
  })
  cols[cond]
}


#' Calculate the Hefindahl Hirschman index for the levels of a factor
#' @param s A factor vector
#' @export
get_HHI <- function(s) {
  sum((table(s) / length(s) * 100) ** 2)
}

#' Return names of columns that are factor, integer or numeric respectively
#'
#' @param df The data.frame to examine
#' @return A list of 4 string vectors of factor, integer and numeric and
#' everything else columns
#' @export
get_col_types <- function(df) {
  cols <- colnames(df)
  types <- sapply(df, class)
  fact_cond <- (types == 'factor') | (types == 'character')
  int_cond  <- types == 'integer'
  num_cond  <- types == 'numeric'
  list(factor_cols  = cols[fact_cond],
       integer_cols = cols[int_cond],
       numeric_cols = cols[num_cond],
       other_cols   = cols[!(fact_cond & int_cond & num_cond)])
}


#' Return column names, whose current data type is integer, but it is potentially
#' categorical.
#' @param df The data.frame
#' @param int_cols A vector of strings, the column nams of integer columns.
#' @param threshold An integer or numeric number. If the number of unique values
#' in the column is less than or equal to threshold, the column names will be returned.
#' @return Names of columns who are integer type but are potentially categorical.
#' @export
cols_int_is_categorical <- function(df, int_cols = NULL, threshold = 2) {
  if (is.null(int_cols)) {
    int_cols <- colnames(df)
  }
  count <- sapply(df[int_cols], function(x) length(unique(x)))
  int_cols[count <= threshold]
}

#' Detect outliers for single column
#'
#' @param s One column of a data.frame
#' @param stategy A string, which strategy to use to identify outlier.It can
#' be 'quantile' or '3sigma (default)'.
#' @param IQR_coef Coefficient of IQR (internal quantile range) in box plot.
#' This parameter is optional and only used for strategy "quantile".The
#' suggested value for this parameter is between 1.5 (default) to 3, with 1.5
#' less conservative and returning all suspected outliers and 3 only returning
#' values the algorithm is more certain to be outliers.
#' @param return_stats If TRUE, the function returns a list with information of
#' if there are outliers, number of outlier, percentage of outlier in the
#' column. If FALSE, the function returns the records themselves who are
#' outliers.
#' @return A boolean to indicate if there are outliers or records that are
#' outliers
#' @export
#'
# Quantile too strick! Fix it. Try to leverage boxplot.stats
detect_outlier_single <- function (s, strategy = "3sigma", IQR_coef = 1.5, return_records = TRUE){
  if (any(is.na(s))) {
    stop("[Error] There are missing values in the column.\n Please drop or impute the column first.")
  }
  if (strategy == "3sigma") {
    outliers <- s[abs(s - mean(s)) > 3 * sd(s)]
  }
  else if (strategy == "quantile") {
    res <- boxplot.stats(s, coef = IQR_coef)
    outliers <- res$out
  }
  else {
    stop("[Error] The supported strategies are: 'quantile' and '3sigma'.")
  }
  if (return_records)
    return(outliers)
  else if (length(outliers))
    return(TRUE)
  else return(FALSE)
}
