#' Bin a numeric variable into a set of equal interval bins.
#'
#' @param x a numeric vector to be split
#' @param num.bins a length one integer vector specifying the number of bins to use
#' @param na.as.missing a flag to indicate whether missing values should be a category
#' or remain as NA values
equalIntervalBins <- function(x, num.bins, na.as.missing = TRUE) {
  ## Address issues with possible missing x and y values
  # Create a set of indicies so that the final field can be sorted properly
  orig_index <- 1:length(x)
  missing_index <- orig_index[is.na(x)]
  available_index <- orig_index[!(orig_index %in% missing_index)]
  new_index <- c(available_index, missing_index)
  a_x <- x[available_index]

  ## Create bins for the available values
  a_bins <- cut(a_x, seq(min(a_x), max(a_x), length.out = (num.bins + 1)), include.lowest = TRUE)

  ## Add back in the missing values
  missing <- if (na.as.missing) {
    rep("Missing", length(missing_index))
  } else {
    rep(NA, length(missing_index))
  }
  all_bins <- as.factor(c(as.character(a_bins), missing))
  all_bins[order(new_index)]
}
