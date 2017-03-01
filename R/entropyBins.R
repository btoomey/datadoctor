intervalLabels <- function(bdf) {
  left_end <- c("[", rep("(", (nrow(bdf) - 1)))
  right_end <- c(rep("]", nrow(bdf)))
  paste0(left_end, format(bdf$first, trim = TRUE, digits = 4), ",", format(bdf$last, digits = 4), right_end)
}

#' Entropy target based bining
#' 
#' \code{entropyBins} finds the optimal bins for a numeric or integer variable
#' to match a factor variable based on entropy
#'
#' @param x a numeric vector to be binned
#' @param y a factor vector to determine the split points
#' @param label.type indicates the type of label levels to use for the levels
#' in the created factor variable. The choices are lower case letters or
#' numeric intervals
#' @param na.as.missing a flag to indicate whether missing values should be a category
#' or remain as NA values
#'
#' @details
#' The entropy bining algorithm makes use of a minimum distance length stopping
#' rule to determine the number of bins to use.
#' @return The function returns a data frame with two columns, the first is
#' the variable name, the second is its importance weight. The rows of the data
#' frame are sorted by the variables' importance weights.
#'
#' @export
entropyBins <- function(x, y, label.type = c("letters", "intervals"), na.as.missing = TRUE) {
  label_type <- match.arg(label.type)
  ## Initial error checking
  # Make sure x and y are as they should be
  if (!is.numeric(x)) {
    stop("The argument x must be a numeric vector.")
  }
  if (!is.factor(y)) {
    stop("The argument y must be a factor vector.")
  }
  if (length(x) != length(y)) {
    stop("The length of x and y must be the same.")
  }

  ## Address issues with possible missing x and y values
  # Create a set of indicies so that the final field can be sorted properly
  orig_index <- 1:length(x)
  missing_index <- orig_index[is.na(x) | is.na(y)]
  available_index <- orig_index[!(orig_index %in% missing_index)]
  new_index <- c(available_index, missing_index)
  a_x <- x[available_index]
  a_y <- y[available_index]

  ## Order the available y and x by the values of x
  x_orig <- a_x
  a_y <- a_y[order(a_x)]
  a_x <- a_x[order(a_x)]
  
  ## The number of available records
  n <- length(a_x)

  ## Find the optimal bins based on entropy and using the MDL stopping rule
  bins <- list(c(a_x[1], a_x[length(a_x)]))
  repeat {
    entry <- 1
    new_bins <- list()
    for (i in bins) {
      first <- i[1]
      last <- i[2]
      this_x <- a_x[a_x >= first & a_x <= last]
      this_y <- a_y[a_x >= first & a_x <= last]
      split <- entropySplit(this_x, this_y, ordered = TRUE)
      # No splits due to insufficient gain or no possible split point
      if (is.null(split$gain)) {
        new_bins[[entry]] <- c(first, last)
        entry <- entry + 1
      } else { # Successful split case
        # The average of the last point of the left and first of the right
        ave_x <- 0.5*(this_x[split$index] + this_x[(split$index + 1)])
        # Add the new splits to the list
        new_bins[[entry]] <- c(first, ave_x)
        new_bins[[(entry + 1)]] <- c(ave_x, last)
        entry <- entry + 2
      }
    }
    # See if the number of bins has grown. If not, then stop, else replace bins
    if (length(new_bins) == length(bins)) {
      break
    }
    # Replace the old bins with the new bins
    bins <- new_bins
  }
  ## Use the bin information to create the categorical variables based on
  ## the available x values
  # Convert the list of bins to a data frame with rows corresponding to bins 
  bin_df <- as.data.frame(t(as.matrix(as.data.frame(bins))))
  names(bin_df) <- c("first", "last")
  row.names(bin_df) <- NULL
  bin_df <- bin_df[order(bin_df$first),]
  # Create the level labels
  if (label_type == "letters") {
    if (nrow(bin_df) < 27) {
      labels <- letters[1:nrow(bin_df)]
    } else {
      num_letter_grps <- ceiling(nrow(bin_df)/26)
      if (num_letter_grps < 677) {
        labels <- character(0)
        for (i in 1:num_letter_grps) {
          labels <- c(labels, paste0(letters[i], letters))
        }
        labels <- labels[1:nrow(bin_df)]
      } else {
        warning("Optimum number of levels exceeds 676 levels, interval bins will be used instead.")
        labels <- intervalLabels(bin_df)
      }
    }
  } else {
    labels <- intervalLabels(bin_df)
  }
  a_bins <- cut(x_orig, breaks = c(-Inf, bin_df$last), include.lowest = TRUE, labels = labels)

  ## Add back in the missing values
  missing <- if (na.as.missing) {
    rep("Missing", length(missing_index))
  } else {
    rep(NA, length(missing_index))
  }
  all_bins <- as.factor(c(as.character(a_bins), missing))
  all_bins[order(new_index)]
}
