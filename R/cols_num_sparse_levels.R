#' @export
cols_num_sparse_levels <- function(df, cols.to.use) {
  # Address possible erroneous columns
  if (any(!(cols.to.use %in% names(df)))) {
    new.cols.to.use <- cols.to.use[cols.to.use %in% names(df)]
    if (length(new.cols.to.use) == 0) {
      stop("None of the items in the vector of column names to use are in the data.")
    }
    missing.cols <- cols.to.use[!(cols.to.use %in% new.cols.to.use)]
    if (length(missing.cols) == 1) {
      warning(paste("The column", missing.cols, "is not in the provided data."))
    } else {
      warning(paste("The columns", formattedTextList(missing.cols), "are not in the provided data."))
    }
  }
  # Reduce the data
  df <- df[, cols.to.use, drop = FALSE]
  # The function to use with lapply
  the_func <- function(x) {
    this_summary <- summary(x[!is.na(x)], maxsum = length(levels(x[!is.na(x)])))
    if (min(this_summary) > 20) {
      return(c(0, 0, 0))
    }
    five_or_less <- sum(as.numeric(this_summary <= 5))
    six_to_ten <- sum(as.numeric(this_summary > 5 & this_summary < 11))
    eleven_to_twenty <- sum(as.numeric(this_summary > 10 & this_summary < 21))
    c(five_or_less, six_to_ten, eleven_to_twenty)
  }
  out_df <- as.data.frame(t(sapply(df, the_func)))
  names(out_df) <- c("five", "ten", "twenty")
  row.names(out_df) <- NULL
  cbind(data.frame(Column = cols.to.use), out_df)
}
