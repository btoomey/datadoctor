#' Find duplicate factor columns in a data frame
#'
#' \code{cols_factor_duplicates} returns a named list, each entry of which is
#' the the set of other columns that represent a duplicate of the named column.
#'
#' @param df A data frame
#'
#' @details
#' Duplicates are found using Goodman-Kurskal tau statistic. Two variables
#' (x1 and x2) are considered to be duplicates if the tau statistic of x1 for
#' x2 is one, and the tau statistic of x2 for x1 is also one. The pairwise
#' comparisons are made with missing values removed, so the comparison is really
#' being made for cases where neither variable has missing values. 
#'
#' @seealso \code{\link{cols_duplicates}}, \code{\link{cols_continuous_duplicates}}
#'
#' @import GoodmanKruskal GKtauDataframe
#'
#' @export
cols_factor_duplicates <- function(df) {
  theClasses <- sapply(df, class)
  theFactors <- names(theClasses)[theClasses == "factor"]
  if (length(theFactors) < 2) {
    return(list())
  }
  df <- df[, theFactors]
  gkMat <- GKtauDataframe(df, includeNA = "no")
  diag(gkMat) <- 0
  dims <- attr(gkMat, "dim")
  colNames <- attr(gkMat, "dimnames")[[2]]
  gkDF <- data.frame(matrix(as.vector(gkMat), nrow = dims[1], ncol = dims[2]))
  names(gkDF) <- colNames
  gkList <- lapply(gkDF, function(x) colNames[x == 1])
  colIndex <- 1:length(colNames)
  gkList2 <- list()
  for (i in colIndex) {
    newIndex <- colIndex[colIndex != i]
    iName <- colNames[i]
    iCols <- gkList[[i]]
    iCols2 <- character(0)
    for (j in newIndex) {
      jName <- colNames[j]
      jCols <- gkList[[j]]
      if ((jName %in% iCols) && (iName %in% jCols)){
        iCols2 <- c(iCols2, jName)
      }
    }
    eval(parse(text = paste0("gkList2$", iName, " <- iCols2")))
  }
  isDuplicate <- sapply(gkList2, function(x) length(x) > 0)
  gkList2[isDuplicate]
}


#' Find duplicate and perfectly correlated numeric and integer columns in a
#' data frame
#'
#' \code{cols_continuous_duplicates} returns a named list, each entry of which is
#' the the set of other columns that represent a duplicate of, or are perfectly
#' correlated with, the named column.
#'
#' @param df A data frame
#'
#' @details
#' Duplicates and perfectly correlated columns are found using the absolute
#' value of pairwise Pearson correlation coefficients. As a result, two
#' that perfectly negatively correlated will be included in the set of returned
#' columns. The pairwise comparisons are made with missing values removed,
#' so the comparison is really being made for cases where neither variable has
#' missing values. 
#'
#' @seealso \code{\link{cols_duplicates}}, \code{\link{cols_factor_duplicates}}
#'
#' @export
cols_continuous_duplicates <- function(df) {
  theClasses <- sapply(df, class)
  theNumbers <- names(theClasses)[theClasses != "factor"]
  if (length(theNumbers) < 2) {
    return(list())
  }
  df <- df[, theNumbers]
  corMat <- abs(cor(df, use = "pairwise.complete.obs"))
  diag(corMat) <- 0
  corDF <- as.data.frame(corMat)
  colNames <- names(corDF)
  corList <- lapply(corDF, function(x) colNames[x > 0.999999999])
  isDuplicate <- sapply(corList, function(x) length(x) > 0)
  corList[isDuplicate]
}


#' Find duplicate and perfectly correlated integer columns in a data frame
#'
#' \code{cols_duplicates} returns a named list, each entry of which is
#' the the set of other columns that represent a duplicate of, or are perfectly
#' correlated with, the named column.
#'
#' @param df A data frame
#'
#' @details
#' The pairwise comparisons are made with missing values removed,so the
#' comparison is really being made for cases where neither variable has
#' missing values. Details of the methods used can be found in the help pages
#' for \link{cols_factors_duplicates} (in the case of factor columns) and
#' \link{cols_factors_duplicates} (in the case of integer and factor columns).
#'
#' @seealso
#' \code{\link{cols_continuous_duplicates}},
#' \code{\link{cols_factors_duplicates}}
#'
#' @export
cols_duplicates <- function(df) {
  if (ncol(df) == 1) {
    return(list())
  }
  theClasses <- sapply(df, class)
  theFactors <- names(theClasses)[theClasses == "factor"]
  theNumbers <- names(theClasses)[theClasses != "factor"]
  factorDuplicates <- cols_factor_duplicates(df[, theFactors])
  numberDuplicates <- cols_continuous_duplicates(df[, theNumbers])
  c(factorDuplicates, numberDuplicates)
}
