#' Find strong bivariate correlations
#'
#' For each continuous variable in a data frame find the variables it is
#' strongly correlated with, where "strong" is user defined.
#'
#' @param df a data frame containing at least two integer or numeric columns
#' @param threshold the value of a correlation (in absolute value terms) that
#' is considered to be strong.
#'
#' @return A names list, with the name corresponding to each variable that has
#' a strong correlation with one or more other variables. Each named item
#' consists of a character vector of the other variables it has a strong
#' relationship with.

strongCor <- function(df, threshold = 0.9) {
  # Input error checking
  if (class(df) != "data.frame") {
    stop('The argument "df" must be a data frame.')
  }
  if (threshold <= 0 || threshold > 1) {
    stop('The argument "threshold" must be in the interval (0, 1].')
  }
  # The column types
  the_types <- get_col_types(df)
  # The names of the integer and numeric column types
  the_cols <- c(the_types$integer_cols, the_types$numeric_cols)
  # Make sure that there are at least two continuous variables in the data
  if (length(the_cols) < 2) {
    stop("There are less than two integer or numeric variables, so correlations cannot be computed.")
  }
  df <- df[, names(df) %in% the_cols]
  # Get the correlation matrix and set the prime diagonal values to zero
  df_cor <- cor(df)
  diag(df_cor) <- 0
  # Get the matrix indicies of the correlations above the threshold
  high_indicies <- which(abs(df_cor) >= threshold, arr.in = TRUE)
  # Terminate if there are no corelations that meet the threshold
  if (nrow(high_indicies) == 0) {
    return(list())
  }
  # Get the unique column index values and the associated variable names
  unique_cols <- unique(high_indicies)
  col_vars <- the_cols[unique_cols]
  # Loop over the unique columns and get the corresponding row indicies and
  # names in order to get a named list of the strongly correlated variables
  out_list <- list()
  for (i in unique_cols) {
    this_name <- the_cols[i]
    row_index <- high_indicies[high_indicies[, 2] == i, 1]
    out_list[[this_name]] <- the_cols[row_index]
  }
  out_list
}
