#' Groups of collinear columns within a data frame
#'
#' \code{cols_collinear} returns a named list with the columns that
#' represent a collinear group.
#'
#' @param df A data.frame that may contain both number (numeric and integer)
#' and factor columns
#'
#' @details
#' The collinear column groups are determined in a three stepp process. In the
#' first, model.matrix is used to convert the data from a data.frame to a
#' matrix where the factors have been converted to a complete set of zero-one
#' indicator variables, and its cross product is calculated. In the second
#' step, singular value decomposition of the cross product in performed to find
#' the total number of collinear groups in the data is found using the number
#' of singular values that are smaller than 1e-7. In the third step the columns
#' of the V matrix from the singular value decompostion that correspond to the
#' very small singular values are subjected to a varimax rotation in order to
#' identify which columns in the data cause the small singular values to occur.
#' The collinear groups are then pruned to remove the trival cases associated
#' with all of the factor levels in the data being placed into a single group
#' and groups whose members correspond to a strict subset of the columns
#' contained in another group. 
#'
#' @return A named list each element of which is a character vector containing
#' the names of the collinear columns.
#'
#' @export

cols_collinear <- function(df) {
  # Address issues associated with factor columns
  factorTest <- sapply(df, function(x) class(x) == "factor")
  theMatrix <- if (all(!factorTest)) {
                 model.matrix(~ . - 1, data = df)
               } else {
                 model.matrix(~ . - 1, data = df, contrasts.arg =
                  lapply(theDF[, factorTest], contrasts, contrasts=FALSE))
               }
  allLevels <- character(0)
  if (any(factorTest)) {
    theFactors <- names(factorTest)[factorTest]
    for (i in theFactors) {
      allLevels <- c(allLevels, paste0(i, eval(parse(text = paste0("levels(df$", i, ")")))))
    }
  }
  # The core portion of the work
  xx <- crossprod(theMatrix)
  sxx <- svd(xx)
  theD <- zapsmall(sxx$d)
  # If there are no collinear groups, return NULL
  if (length(theD[theD == 0]) == 0) {
    return(NULL)
  }
  zeroD <- (1:length(theD))[theD == 0] 
  vmV <- varimax(sxx$v[, zeroD])
  theLoadings <- vmV$loadings
  loadingVec <- as.vector(theLoadings)
  loadingVec[abs(loadingVec) < 0.1] <- 0
  loadingDim <- attr(theLoadings, "dim")
  loadingMat <- matrix(loadingVec, nrow = loadingDim[1], ncol = loadingDim[2])
  colNames <- attr(theMatrix, "dimnames")[[2]]
  collinearList <- lapply(as.data.frame(loadingMat), function(x) colNames[x != 0])
  # Remove the uninteresting case of all factor levels falling into a group
  if(length(allLevels) > 0) {
    allLevelsNum <- numeric(0)
    for (i in 1:length(collinearList)) {
      if (all(allLevels %in% collinearList[[i]])) {
        allLevelsNum <- c(i, allLevelsNum)
      }
    }
    if (length(allLevelsNum) > 0) {
      collinearList <- collinearList[-allLevelsNum]
    }
  }
  # Remove instances of the uniteresting case where one group is wholely in
  # another group.
  subGroupNum <- numeric(0)
  for (i in 1:(length(collinearList) - 1)) {
    start <- i + 1
    for (j in start:length(collinearList)) {
      if (all(collinearList[[j]] %in% collinearList[[i]])) {
        subGroupNum <- c(subGroupNum, j)
      }
    }
  }
  if (length(subGroupNum) > 0) {
    subGroupNum <- subGroupNum[order(subGroupNum, decending = TRUE)]
    for (i in subGroupNum) {
      collinearList <- collinearList[-i]
    }
  }
  # Prep the final output
  names(collinearList) <- paste0("Group", 1:length(collinearList))
  collinearList
}
