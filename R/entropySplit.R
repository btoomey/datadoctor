#' Finds the value of a numeric vector that results in the best split based on
#' a factor variable
#'
#' @param x a numeric vector to be split
#' @param y a factor vector to determine the split point
#' @param ordered a logical which indicates whether x and y have been ordered by the values of x
#' @param possible_splits a vector of indicies of possible split points, which is calculated if its value is NULL
entropySplit <- function(x, y, ordered = FALSE) {
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

  ## Prep data values
  # Order y and x by the values of x if not already done
  if (!ordered) {
    y <- y[order(x)]
    x <- x[order(x)]
  }
  # The common length of x and y
  n <- length(x)

  ## Find the index values of the possible split points
  possible_splits <- which(y[1:(n - 1)] != y[2:n])
  # If there are no possible splits than return from the function early
  if (length(possible_splits) == 0){
    return(list(index = NA, value = NA, entropy = NA, gain = NULL))
  }
  # Remove possible splits where the value of x is the same on both sides
  possible_splits <- possible_splits[x[possible_splits] != x[(possible_splits + 1)]]
  # If there there are no possible splits, then terminate execution
  if (length(possible_splits) == 0) {
    out_list <- list(index = NA, value = NA, entropy = NA, gain = NULL)
    class(out_list) <- "entropy.split"
    return(out_list)
  }

  ## Calculate the entropy values of the possible split points
  # The function used in sapply to calculate the entropy points
  # Note: the use of left/right is based on a decision tree branch
  entropyCalc <- function(ps) {
    left_x <- x[1:ps]
    left_pct <- length(left_x)/length(x)
    left_y <- y[1:ps]
    right_y <- y[(ps + 1):n]
    left_pct*entropy(left_y, base = 2) + (1 - left_pct)*entropy(right_y, base = 2)
  }
  # Calculate the entropy values
  entropy_values <- sapply(possible_splits, entropyCalc)

  ## Calculate the minimum distance length gain for the split and it threshold
  # The average entropy for the split and the index value of the split
  min_entropy <- min(entropy_values)
  # In case of ties the lowest index value is taken
  split_index <- possible_splits[entropy_values == min_entropy][1]
  # Calculate the MDL gain
  base_entropy <- entropy(y, base = 2)
  left <- 1:split_index
  right <- (split_index + 1):n
  gain <- base_entropy - min_entropy
  y_left <- y[left]
  y_right <- y[right]
  k <- length(levels(y))
  k1 <- length(levels(y_left))
  k2 <- length(levels(y_right))
  delta <- posLog(3^k - 2, base = 2) - (k*base_entropy - k1*entropy(y_left, base = 2) - k2*entropy(y_right, base = 2))
  threshold <- posLog(n - 1, base = 2)/n + delta/n

  ## Prepare and output the results
  out_list <- if (gain >= threshold) {
    list(index = split_index, value = x[split_index], entropy = min_entropy, gain = gain)
  } else {
    list(index = split_index, value = x[split_index], entropy = min_entropy, gain = NULL)
  }
  class(out_list) <- "entropy.split"
  out_list
}
