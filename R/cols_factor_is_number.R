strip_helper <- function (x) {
    xx <- trimws(as.character(x))
    no_na <- xx[nchar(xx) > 0 & !is.na(xx)]
    #if (any(!nchar(xx)))
    #    return(NA)
    xx <- gsub("[\\,\\$]", "", xx)
    if (all(substr(no_na, nchar(no_na), nchar(no_na)) == "%")) {
        xx <- as.numeric(substr(xx, 1, nchar(xx) - 1))/100
    }
    else if (any(grepl("\\.", no_na))) {
        xx <- as.numeric(xx)
    }
    else {
        xx <- as.integer(xx)
    }
    xx
}

#' Determine which factors might actually be numbers.
#'
#' @param df a data frame
#' @param factor.cols a vector containing the names of the factor variables
#'  of interest
#' @export
cols_factor_is_number <- function(df, factor.cols) {
  # Reduce the data
  this_data <- df[, factor.cols, drop = FALSE]
  # The function for sapply, it returns a boolean vector
  the_func <- function(x) {
    suppressWarnings(strip_x <- strip_helper(x))
    if (sum(as.numeric(is.na(strip_x))) > sum(as.numeric(is.na(x)))) {
      return(FALSE)
    }
    uniq_x <- unique(strip_x[!is.na(strip_x)])
    uniq_x <- uniq_x[order(uniq_x)]
    if (length(uniq_x) < 3) {
      return(FALSE)
    }
    if (length(uniq_x) < 11) {
      sequential <- all(uniq_x[2:length(uniq_x)] - uniq_x[1:(length(uniq_x) - 1)] == 1)
      if (sequential) {
        return(FALSE)
      }
    }
    TRUE
  }
  # Test whether each factors appears to actually be numeric
  test_numeric <- sapply(this_data, the_func)
  names(test_numeric) <- factor.cols
  test_numeric
}

change_factor_to_number <- function (df, fact_cols)
{
    df[fact_cols] <- as.data.frame(apply(df[fact_cols], 2, strip_helper))
    df
}
