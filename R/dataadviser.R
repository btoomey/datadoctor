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

#============================================================================
# Variability analysis:
#       - Numeric columns: check if standard deviation < threshold
#       - Categorical columns: check if minority class percentage < threshold
#============================================================================
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


#' Return names of columns who has the same value over the threshold
#'
#' @param df The data.frame to examine
#' @param cat_cols The name of columns you wish to examine
#' @param threshold The threshold of same value percentage in a column,
#' 95\% by default
#' @return A vector of column names who has many same values
#' @export
cols_with_many_same <- function(df, cat_cols = colnames(df), threshold = 0.95) {
  vals <- lapply(df[cat_cols], function(x){
    t <- table(x)
    names(t[t == max(t)])
  })
  percent_same_val <- colSums(mapply(function(x,y){x == y}, df[cat_cols], vals)) / NROW(df)
  names(percent_same_val)[percent_same_val >= threshold]
}

#' Return names of columns who have low standard deviation
#'
#' @param df The data.frame to examine
#' @param num_cols The name of columns of numeric type to examine
#' @param threshold The threshold of stardard deviation that is considered as
#' too low.
#' @param scale can be FALSE, 'zeroOne' or 'IQR'. If scale is 'zeroOne', the
#' data to the interval of [0,1] before analyzing its standard deviation; if
#' scale is 'IQR', it returns standard deviation divided by (Q3 - Q1)
#' @return A vector of column names with low standard deviation
#' @export
cols_with_low_sd <- function(df, num_cols = NULL,
                             threshold = 0.01, scale = FALSE) {
  if (is.null(num_cols)) {
    num_cols <- colnames(df)
  }
  if (scale == 'zeroOne') {
    m0 <- sapply(df[num_cols], min)
    m1 <- sapply(df[num_cols], max)
    temp <- (df[num_cols] - m0) / (m1 - m0)
    sds <- sapply(temp, function(x){sd(x) <= threshold})
  } else if (scale == 'IQR'){
    sds <- sapply(df[num_cols], function(x){sd(x) / IQR(x) <= threshold})
  } else{
    sds <- sapply(df[num_cols], function(x){sd(x) <= threshold})
  }
  names(sds)[sds]
}

#' Helper function to compute HHI
get_HHI <- function(s) {
  sum((table(s) / length(s) * 100) ** 2)
}
#' Return names of columns with high Herfindahl-Hirschman index. This is only
#' for categorical predictors.
#'
#' @param df The data.frame to examine
#' @param fact_cols The names of factor columns to examine
#' @param threhold The HHI threshold. Note that HHI is in the range (0, 10000].
#' The higher the HHI, the more monopoly some levles have.
#' @return A vector of column names with high HHI
#' @export
cols_with_high_hhi <- function(df, fact_cols, threshold = 9000) {
  HHI <- sapply(df[fact_cols], get_HHI)
  fact_cols[HHI >= threshold]
}

#' Helper function to calculate near zero variance
nzv_helper <- function(s) {
  temp <- sort(table(s))
  unique_percent <- length(temp) / length(s)
  top2_ratio <- tail(temp,1) / tail(temp,2)[1]
  return(list(unique = unique_percent, freq = top2_ratio))
}

#' Return names of columns with near zero variance.
#'
#' @param df The data.frame to examine
#' @param num_cols The names of columns to examine
#' @param uniqueCut Percentage of allowed unique values.
#' @param freqCut Ratio allowed of the most and the second most frequent values
#' @return A vector of column names with near zero variance
#' @export
cols_with_nzv <- function(df, num_cols = NULL, uniqueCut = 10, freqCut = 19) {
  if (is.null(num_cols)) {
    num_cols <- colnames(df)
  }
  cond <- sapply(df, function(x){
    res <- nzv_helper(x)
    (res$unique * 100 <= uniqueCut) & (res$freq >= freqCut)
  })
  num_cols[cond]
}

#' Return names of columns that are possibly ID columns
#'
#' @param df The data.frame to examine
#' @param cols The names of columns to examine
#' @param threshold The threshold of possible id by percentage of unique
#' numbers, the default is 0.95
#' @return Names of columns that are likely to be ID columns
#' @export
cols_possible_id <- function(df, cols, threshold = 0.95) {
  n <- NROW(df)
  temp <- sapply(df[cols], function(x){
    (class(x) %in% c('factor','character', 'integer')) & length(unique(x)) > threshold*n
  })
  cols[temp]
}

#' Return names of categorical columns with very sparse levels or very unbalanced.
#'
#' @param df The data.frame to examine
#' @param fact_cols The names of factor columns to examine
#' @param freqCut The threshod of the ratio of the most frequent level devided
#' by the second most frequent level
#' @param numberCut The threshold of the number of least frequent level.
#' @return Names of columns that has too sparse or unbalanced levels
#' @export
cols_sparse_categorical <- function(df, fact_cols, freqCut = 19,
                                    numberCut = 10) {
  cond <- sapply(df[fact_cols], function(x){
    t1 <- sort(table(x))
    t2 <- tail(t1,2)
    top2_ratio <- t2[2] / t2[1]
    (top2_ratio >= freqCut) & (t1[1] < numberCut)
  })
  fact_cols[cond]
}

#' Return names of categorical columns with too many levels to be useful.
#'
#' @param df The data.frame to examine
#' @param cat_cols The names of categorical columns to examine
#' @param threshold The threshold of number of levels as percentage of record
#' number. The default is 50\%
#' @return Names of categorical columns with too many levels
#' @export
cols_with_many_levels <- function(df, cat_cols = NULL, thres_percent = 0.5,
                                  thres_absolute = 100) {
  if (is.null(cat_cols))
    cat_cols <- colnames(df)
  threshold <- min(NROW(df)*thres_percent, thres_absolute)
  cond <- sapply(df[cat_cols], function(x){length(unique(x)) >= threshold})
  cat_cols[cond]
}


#============================================================
# Column type analysis:
#       - Is numeric really numeric?
#             1) Float may be integer : 1.0, 2.0, 3.0 ...
#             2) Integer may be categorical: 1,1,1,0,0,0,1
#       - Is string really string?
#             1) Percentage: 23%
#             2) Currency:  $2323.23
#             3) Comma seperated number: 1,234
#             4) A combination of 2) and 3): $1,235.29
#=============================================================
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


float_is_int <- function(x) {
  all(as.integer(x) - x == 0 | is.na(x))
}

#' Change float column to int, if it essentially is an integer type
#' @param df The data.frame
#' @param float_cols The columns that is of float type that should be potentially changed
#' @return Modified data.frame
#' @export
change_float_to_int <- function(df, float_cols) {
  cols <- sapply(df[float_cols], float_is_int)
  df[cols] <- apply(df[cols], 2, as.integer)
  df
}

#' Return column names, whose current data type is integer, but it is potentially
#' categorical.
#' @param df The data.frame
#' @param int_cols A vector of strings, the column nams of integer columns.
#' @param threshold An integer or numeric number. If the number of unique values
#' in the column is less than or equal to threshold, the column names will be returned.
#' @return Names of columns who are integer type but are potentially categorical.
#' @export
cols_int_is_categorical <- function(df, int_cols, threshold = 2) {
  count <- sapply(df, function(x) length(unique(x)))
  names(count[count <= threshold])
}


possible_nums_helper <- function(x) {
  xx <- trimws(x)
  cond <- grepl("\\d*\\.*\\d+\\%$", xx)
  cond <- cond | grepl('^\\$\\s*\\d*\\.*\\d{0,2}$', xx)
  cond <- cond | grepl('(?:\\,\\d{3}$|^\\d{0,3}$)', xx)
  cond
}

# Return column names if column item are in the following format:
#       * ' xx.x%   '
#           ==> Note: The regex search should return True/False accordingly
#                     for the following case:
#           '12.323%': True,
#           '12%'    : True,
#           '12%33'  : False,
#           'asb%'   : False}
#       * '  $xxx '
#         ==> Note: The regex search should return True/False accordingly
#                   for the following case:
#           ' $123   ' : True
#           '$ 12.345' : True
#           '$ 12,345' : True  -- TODO
#       * ' 12,347':
#         ==> Note: The regex search should return True/False accordingly
#                   for the following case:
#          '123'          : True,
#          '1,123'        : True,
#          '1,123,123,123': True,
#          '1,1234'       : False,
#          '1234'         : False
#       * ' $1,234 '

#' Return the names of factor columns who are likely to be numeric.
#'
#' @param df The data.frame
#' @param cat_cols A vector of strings, names of categorical columns
#' @param threshold An integer or numeric number. If there are more than
#' threshold or 10% number of certain patterns we know they are numeric,
#' return the column names.
#' @return Names of factor type columns who are likely to be numerical.
#' @export
cols_possible_nums <- function(df, cat_cols, threshold = 5) {
  threshold <- min(threshold, as.integer(NROW(df) / 10))
  count <- lapply(df, possible_nums_helper)
  count <- sapply(count, function(x) sum(x))
  names(count[count > threshold])
}


#======================================================================
# Extreme value / Outlier analysis:
#   - Numeric columns:
#       1. Single column: Quantile
#       2. Single column: 3-sigma
#       3. Multiple column: Mahalanobis distance TODO
#       4. Multiple column: K-nearest neighbor TODO
#   - String columns:
#       1. Outlier -- unbalanced TODO
#=======================================================================

#' Detect outliers for single variable
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
detect_outlier_single <- function(s, strategy = '3sigma',
                                  IQR_coef =  1.5,
                                  return_stats = TRUE) {
  if (any(is.na(s))) {
    stop("[Error] There are missing values in the column.
         Please drop or impute the column first.")
  }

  if (strategy == '3sigma') {
    outliers <- s[abs(s-mean(s)) > 3*sd(s)]
  }
  else if (strategy == 'quantile') {
    res <- boxplot.stats(s, coef = IQR_coef)
    outliers <- res$out
  } else {
    stop("[Error] The supported strategies are: 'quantile' and '3sigma'.")
  }

  nOutliers <- length(outliers)
  if (nOutliers)
    outlier_exist <- TRUE
  else
    outlier_exist <- FALSE

  if (return_stats)
    return(list(exist = outlier_exist,
                numberOutlier = nOutliers,
                percentageOutlier = nOutliers / length(s)))
  else
    return(outliers)
  }


detect_outlier <- function(df, cols = NULL, strategy = '3sigma',
                           IQR_coef =  1.5,
                           return_stats = TRUE) {
  if (is.null(cols))
    cols <- colnames(df)

  if (any(sapply(df[cols], is.na))) {
    stop("[Error] There are missing values in the columns.
         Please drop or impute the column first.")
  }

  if (strategy == '3sigma') {
    outliers <- s[abs(s-mean(s)) > 3*sd(s)]
  }
  else if (strategy == 'quantile') {
    res <- boxplot.stats(s, coef = IQR_coef)
    outliers <- res$out
  } else {
    stop("[Error] The supported strategies are: 'quantile' and '3sigma'.")
  }

  nOutliers <- length(outliers)
  if (nOutliers)
    outlier_exist <- TRUE
  else
    outlier_exist <- FALSE

  if (return_stats)
    return(list(exist = outlier_exist,
                numberOutlier = nOutliers,
                percentageOutlier = nOutliers / length(s)))
  else
    return(outliers)
  }

#' Return the names of columns who have outliers.
#'
#' @param df The data.frame
#' @param num_cols A vector of string, the names of numeric columns you wish to
#' identify if they have outliers.
#' @param strategy A string, which strategy to use to identify outlier.It can
#' to 'quantile'(default) or '3sigma'.
#' @return Names of numeric columns who are likely to have outliers
#' @export
cols_numeric_outliers <- function(df, num_cols, strategy = "quantile") {
  cond <- sapply(df[num_cols], detect_outlier_single, strategy = strategy)
  return(num_cols[cond])
}
