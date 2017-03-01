#' Conservatively detect outliers for continuous variables in a data frame
#'
#' @param df A data.frame
#' @param num.col A vector of the names of the numeric variables in the
#' data.frame
#' @param method A string, specifying the .It can
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
cols_num_outliers <- function(df, num.cols, method = match.arg(c("boxplot", "3sigma", "4sigma"))) {
  df <- df[, num.cols, drop = FALSE]
  the_func <- function(x, method) {
    x <- x[!is.na(x)]
    if (method == "boxplot") {
      n_outliers <- length(boxplot.stats(x)$out)
      if (all(x > 0) && skewness(x) > 1.5) {
        n_outliers <- min(n_outliers, length(boxplot.stats(log(x))$out))
      }
    } else {
      sd_val <- if (method == "3sigma") {
          3
        } else {
          4
        }
      n_outliers <- length(x[abs(x - mean(x)) > sd_val*sd(x)])
      if (all(x > 0) && skewness(x) > 1.5) {
        n_outliers <- min(n_outliers, length(x[abs(log(x) - mean(log(x))) > sd_val*sd(log(x))]))
      }
    }
    if (length(n_outliers) == 0) {
      n_outliers <- 0
    }
    n_outliers
  }

  sapply(df, the_func, method = method)
}
