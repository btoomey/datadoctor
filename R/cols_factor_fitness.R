#' @export
cols_factor_fitness <- function(fm) {
  # Penalty for missing values
  missing <- fm$Percent_Missing_Blank
  missing_penalty <- rep(0, nrow(fm))
  missing_penalty[missing > 0 & missing <= 2.0] <- 0.1
  missing_penalty[missing > 2.0 & missing <= 5.0] <- 0.2
  missing_penalty[missing > 5.0 & missing <= 10.0] <- 0.3
  missing_penalty[missing > 10.0 & missing <= 20.0] <- 0.4
  missing_penalty[missing > 20.0 & missing <= 30.0] <- 0.5
  missing_penalty[missing > 30.0 & missing <= 40.0] <- 0.6
  missing_penalty[missing > 40.0 & missing <= 50.0] <- 0.7
  missing_penalty[missing > 50.0 & missing <= 60.0] <- 0.8
  missing_penalty[missing > 60.0] <- 0.9
  # Penalty for too many levels
  too_many_penalty <- rep(0, nrow(fm))
  too_many_penalty[fm$num_levels > 32 & fm$num_levels <= 40] <- 0.1
  too_many_penalty[fm$num_levels > 40 & fm$num_levels <= 55] <- 0.2
  too_many_penalty[fm$num_levels > 55] <- 0.3
  # Penalty for sparse levels
  sparse_level_penalty <- 0.05*fm$five + 0.02*fm$ten + 0.01*fm$twenty
  # Calculate the fitness score
  fitness <- 1 - missing_penalty - too_many_penalty - sparse_level_penalty
  fitness[fitness < 0] <- 0
  fitness
}
