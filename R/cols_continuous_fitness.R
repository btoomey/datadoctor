#' @export
cols_continuous_fitness <- function(cm) {
  # Penalty for missing values
  missing <- cm$Percent_Missing_Blank
  missing_penalty <- rep(0, nrow(cm))
  missing_penalty[missing > 0 & missing <= 2.0] <- 0.1
  missing_penalty[missing > 2.0 & missing <= 5.0] <- 0.2
  missing_penalty[missing > 5.0 & missing <= 10.0] <- 0.3
  missing_penalty[missing > 10.0 & missing <= 20.0] <- 0.4
  missing_penalty[missing > 20.0 & missing <= 30.0] <- 0.5
  missing_penalty[missing > 30.0 & missing <= 40.0] <- 0.6
  missing_penalty[missing > 40.0 & missing <= 50.0] <- 0.7
  missing_penalty[missing > 50.0 & missing <= 60.0] <- 0.8
  missing_penalty[missing > 60.0] <- 0.9
  # Penalty for skewness
  skewness_penalty <- rep(0, nrow(cm))
  skewness_penalty[cm$Skewness >= 1.5 & cm$Skewness < 3] <- 0.1
  skewness_penalty[cm$Skewness >= 3 & cm$Skewness < 5] <- 0.2
  skewness_penalty[cm$Skewness >= 5] <- 0.3
  # Penalty for outliers
  outlier_penalty <- rep(0, nrow(cm))
  outlier_penalty[cm$Pct_Outliers >= 5 & cm$Pct_Outliers < 10] <- 0.1
  outlier_penalty[cm$Pct_Outliers >= 10 & cm$Pct_Outliers < 20] <- 0.2
  outlier_penalty[cm$Pct_Outliers >= 20] <- 0.3
  # Calculate the fitness score
  fitness <- 1 - missing_penalty - skewness_penalty - outlier_penalty
  fitness[fitness < 0] <- 0
  fitness
}
