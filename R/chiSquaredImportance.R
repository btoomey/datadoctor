#' Chi-squared statistic based variable importance weights
#'
#' \code{chiSquaredImportance} returns a data frame of variable importance
#' weights for a set of predictor variables for a factor target variable.
#'
#' @param formula an R formula to specify the categorical target to match and the
#' numeric or integer predictors
#' @param data a data frame (or matrix that will be coereced to a data frame)
#' that contains the variables specified in the formula
#'
#' @details
#' The potential predictors can be factor, integer, or numeric variables.
#' Integer and numeric variables are binned into factors using an entropy
#' based algorithm to find the best set of bins relative to the target
#' variable. The algorithm makes use of a minimum distance length stopping
#' rule determine the number of bins to use.
#'
#' @return The function returns a data frame with three columns: the variable
#' names, the importance weights, and (for binary targets) a linearity measure.
#' The rows of the data frame are sorted by the variables' importance weights.
#'
#' @export

chiSquaredImportance <- function(formula, data) {
  ## Initial error checking and coercion of the input data
  if (class(formula) != "formula") {
    stop("The provided formula is not a formula object.")
  }
  if (class(data) != "data.frame") {
    if (class(data) != "matrix") {
      stop("The provided data must be a data frame or a matrix")
    } else {
      data <- as.data.frame(data)
    }
  }

	## Get the target (a factor) and the possible numeric predictors
  the_formula <- as.character(formula)
  target_name <- the_formula[2]
	predictor_names <- trimNewlines(unlist(strsplit(the_formula[3], split = " \\+ ")))
  # The target should be a factor variable, if not it is equal interval binned
  the_target <- data[, target_name]
  if (!is.factor(the_target)) {
    stop("The y variable must be a factor")
  }
  the_predictors <- data[, predictor_names, drop = FALSE]
	rm(data)
	
	## The continuous predictors need to be entropy binned, so determine which
	## predictors are continuous and address them appropriately
	# Determine the class of each potential predictor
	the_classes <- sapply(the_predictors, class)
  # Split the continuous and categorical predictors into two groups
	continuous_preds <- the_predictors[, names(the_classes)[the_classes == "numeric" | the_classes == "integer"], drop = FALSE]
	categorical_preds <- the_predictors[, names(the_classes)[the_classes == "factor"], drop = FALSE]
	# Entropy bin any continuous predictors
	if (ncol(continuous_preds) > 0) {
		bin_preds <- as.data.frame(sapply(continuous_preds, entropyBins, y = the_target))
		names(bin_preds) <- names(continuous_preds)
		the_predictors <- cbind(bin_preds, categorical_preds)
		rm(bin_preds)
	}
	if (length(summary(the_target)) == 2 && ncol(continuous_preds) > 0) {
		rho <- sapply(continuous_preds, function(x) cor(x, as.integer(the_target)))
		rho <- c(abs(rho), rep(NA, ncol(categorical_preds)))
	} else {
		rho <- rep(NA, ncol(continuous_preds) + ncol(categorical_preds))
	}
	# Remove unneeded data frames and garbage collect
	rm(list = c("continuous_preds", "categorical_preds"))
	gc()

  ## The chi-squared importances
	# The function that calculates the importance weight for each
	chiSqImp <- function(x, y) {
		actual_counts <- table(y, x)
		row_sums <- apply(actual_counts, 1, sum)
		col_sums <- apply(actual_counts, 2, sum)
		n <- length(y)
		expected_counts <- t(as.matrix(col_sums) %*% t(as.matrix(row_sums))) / n
		chi_sq <- sum((actual_counts - expected_counts)^2 / expected_counts)
	  phi <- if(chi_sq == 0 || length(col_sums) < 2 || length(row_sums) < 2) {
			  0
			} else {
				sqrt(chi_sq / (n * min(length(col_sums) - 1, length(row_sums) - 1)))
			}
    phi
	}
  # Calculate the importance weights
	importance_wts <- sapply(the_predictors, chiSqImp, y = the_target)

	## Prep and provide the output
	out_df <- data.frame(Variable = names(the_predictors), Importance = importance_wts, Linearity = rho)
	out_df <- out_df[order(out_df$Importance, decreasing = TRUE),]
	row.names(out_df) <- NULL
	out_df
}
