#' Correlation based variable importance weights using B-splines for continuous
#' predictors to find non-linear effects
#'
#' \code{bsplineImportance} returns a data frame of variable importance
#' weights and a measure of monotonicity for a set of predictor variables
#' for a continuous target variable.
#'
#' @param formula an R formula to specify the categorical target to match and the
#' numeric or integer predictors
#' @param data a data frame (or matrix that will be coerced to data frame) that
#' contains the variables specified in the formula
#' @param df the degrees for freedom to used in the B-spline function of the
#' potential predictors, with a default set at five
#'
#' @details
#' Determine the strength of the relationship between a target and a potential
#' predictor, based on the correlation between fitted actual values of a linear
#' regression between the B-spline transformed predictor and the target, and
#' whether the relationship appears to be linear based on the ratio of the
#' Pearson correlation coeficient between the a potential predictor and the
#' target over the importance measure. This measure is bounded between zero and
#' and one, with higher values indicating a more linear relationship.
#'
#' @return The function returns a data frame with three columns, the first is
#' the variable name, the second is its importance weight, and the third is the
#' linearity measure for continuous predictors, NA for categorical
#' predictors.
#'
#' @export
bsplineImportance <- function(formula, data, df = 5) {
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

  ## Get the names of the target (continuous) and the predictors
  the_formula <- as.character(formula)
  target_name <- the_formula[2]
  target_class <- class(data[, target_name])
  # Make sure the target is continuous
  if (target_class != "numeric" && target_class != "integer") {
    stop("The y variable for the analysis needs to be either numeric or integer.")
  }
  # Remove records from the data where the target is missing
  data[!is.na(data[, target_name]),]
  # The names of the potential predictors
  predictor_names <- trimNewlines(unlist(strsplit(the_formula[3], split = " \\+ ")))

  ## The function for sapply that does the analysis
  fitActualCor <- function(x_name, y_name, dframe, dfn) {
    # The data with missing values removed
    this_data <- na.omit(dframe[,c(y_name, x_name)])
    # If the predictor is continuous use a b-spline to determine the importance
    if (class(this_data[, x_name]) == "numeric" || class(this_data[, x_name]) == "integer") {
      this_formula <- eval(parse(text = paste0(y_name, " ~ ", "bs(", x_name, ", df = ", dfn,")")))
      this_model <- lm(this_formula, data = this_data)
      cor_star <- cor(this_model$fitted.values, this_data[, y_name])
      cor_actual <- cor(this_data[, x_name], this_data[, y_name])
      # The linearity measure
      nm <- abs(cor_actual)/cor_star
      out_vec <- c(cor_star, nm)
    } else {
      this_formula <- eval(parse(text = paste0(y_name, " ~ ", x_name)))
      this_model <- lm(this_formula, data = this_data)
      cor_star <- cor(this_model$fitted.values, this_data[, y_name])
      out_vec <- c(cor_star, NA)
    }
  }

  ## Do the analysis using via sapply and output the results
  results <- lapply(predictor_names, fitActualCor, y_name = target_name, dframe = data, dfn = df)
  results <- as.data.frame(t(as.matrix(as.data.frame(results))))
  row.names(results) <- NULL
  names(results) <- c("Importance", "Linearity")
  results <- cbind(data.frame(Variable = predictor_names), results)
  results[order(results$Importance, decreasing = TRUE),]
}
