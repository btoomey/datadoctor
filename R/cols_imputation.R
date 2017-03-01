#' @export
cols_imputation <- function(df) {
  for (i in names(df)) {
    this_column <- df[[i]]
    if (any(is.na(this_column))) {
      if (is.factor(this_column)) {
        this_column <- as.character(this_column)
        this_column[is.na(this_column)] <- "Missing"
        df[[i]] <- as.factor(this_column)
      } else {
        missing_indicator <- rep("No", length(this_column))
        missing_indicator[is.na(this_column)] <- "Yes"
        missing_indicator <- as.factor(missing_indicator)
        missing_column <- paste0(i, "_Missing")
        eval(parse(text = paste0("df$", missing_column, " <- missing_indicator")))
        if (min(this_column[!is.na(this_column)]) > 0) {
          this_column[is.na(this_column)] <- 0
        } else {
          this_column[is.na(this_column)] <- floor(min(10*min(this_column[!is.na(this_column)]), min(this_column[!is.na(this_column)]) - 100))
        }
        df[[i]] <- this_column
      }
    } else {
      df[[i]] <- df[[i]]
    }
  }
  df
}
